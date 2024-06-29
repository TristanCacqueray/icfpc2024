module Main where

import RIO
import RIO.ByteString.Lazy qualified as Bytes
import RIO.Char
import RIO.Text qualified as Text
import RIO.List.Partial qualified as Partial
import RIO.Text.Partial qualified as PartialText

import Control.Monad.Trans.Writer.CPS
import Data.Coerce
import Data.Text.IO qualified as Text
import System.FilePath
import Test.QuickCheck.Instances.Natural ()
import Test.QuickCheck.Instances.Text ()
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.QuickCheck
import System.IO.Error

import ProgCon.API
import ProgCon.Eval
import ProgCon.Parser
import ProgCon.Printer qualified as Printer
import Spaceship qualified
import qualified LambdaMan
import qualified Codegen

main :: IO ()
main = writerMain do
  testWriter "fast checks" fastChecks
  testWriter "slow checks" slowChecks

data ProblemDefinition = ProblemDefinition
  { name :: String
  , size :: Natural
  , solve :: Expr -> Either String Expr
  , validate :: Expr -> Either String Float
  }

problems :: [ProblemDefinition]
problems =
  [ ProblemDefinition {name = "spaceship", size = 25, solve = Spaceship.solveExpression, validate = Spaceship.validateExpression}
  , ProblemDefinition {name = "lambdaman", size = 21, solve = LambdaMan.solveExpression, validate = LambdaMan.validateExpression}
  ]

fastChecks :: Writer ([TestTree] -> [TestTree]) ()
fastChecks = do
  testWriter "parser" do
    writeProperty "string" do parseExpr "S'%4}).$%8" === Right (EStr "get index")
    writeProperty "two abstractions and two applications" do
      parseExpr "B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L\" L# ? B= v# I! I\" B$ L$ B+ B$ v\" v$ B$ v\" v$ B- v# I\" I%"
        === Right (EBinary '$' (EBinary '$' (ELam 1 (EBinary '$' (ELam 2 (EBinary '$' (EVar 1) (EBinary '$' (EVar 2) (EVar 2)))) (ELam 2 (EBinary '$' (EVar 1) (EBinary '$' (EVar 2) (EVar 2)))))) (ELam 1 (ELam 2 (EIf (EBinary '=' (EVar 2) (EInt 0)) (EInt 1) (EBinary '$' (ELam 3 (EBinary '+' (EBinary '$' (EVar 1) (EVar 3)) (EBinary '$' (EVar 1) (EVar 3)))) (EBinary '-' (EVar 2) (EInt 1))))))) (EInt 4))
    writeProperty "decodeString ∘ encodeString = id" \(AppropriateText text) -> (decodeString . encodeString) text === text
    writeProperty "encodeString ∘ decodeString = id" \(AppropriateEncodedText text) -> (encodeString . decodeString) text === text
    writeProperty "str2int ∘ int2str = id" \number -> (str2int . int2str) number === number
    writeProperty "int2str ∘ str2int = id" \(AppropriateEncodedText text) ->
      text == "!" || (not . Text.null) text && (fmap fst . Text.uncons) text /= Just '!' ==> (int2str . str2int) text === text
  testWriter "evaluator" do
    writeProperty "hello" do
      evalExpr
        emptyEnvironment
        (EBinary '$' (EBinary '$' (ELam 2 (ELam 3 (EVar 2))) (EBinary '.' (EStr "Hello") (EStr " World!"))) (EInt 42))
        === Right (EStr "Hello World!")
  testWriter "solutions" do
    forM_ problems fastCheckProblem

fastCheckProblem :: ProblemDefinition -> Writer ([TestTree] -> [TestTree]) ()
fastCheckProblem ProblemDefinition {..} =
  testWriter name do
    forM_ [1 .. size] \number ->
      writeProperty (show number) do
        (within 1_000_000 . ioProperty) do
          problemExpression <- getExpressionFromFile ("examples" </> name </> show number </> "problem.expression")
          pure do isRight do validate problemExpression

slowChecks :: Writer ([TestTree] -> [TestTree]) ()
slowChecks = do
  testWriter "solutions" do
    forM_ problems checkProblem

checkProblem :: ProblemDefinition -> Writer ([TestTree] -> [TestTree]) ()
checkProblem ProblemDefinition {..} = testWriter name do
  forM_ [1 .. size] \number ->
    sequentialTestWriter (unwords ["problem", show number]) do
      communicateProblem name number
      getProblemExpression name number
      when (name == "lambdaman") do getMap number
      getSolutionExpression name number solve
      communicateSolution name number
      checkCorrectness name number
      checkCost name number

problemPath :: FilePath -> Natural -> FilePath
problemPath name number = name </> show number

communicateProblem :: String -> Natural -> Writer ([TestTree] -> [TestTree]) ()
communicateProblem name number =
  checkCommunication
    problem
    "communicate problem"
    do pure do EStr (Text.unwords ["get", Text.pack name <> (Text.pack . show) number])
 where
  problem = problemPath name number

getProblemExpression :: String -> Natural -> Writer ([TestTree] -> [TestTree]) ()
getProblemExpression name number =
  write do
    let source = "examples" </> problem </> "communicate problem" </> "response.expression.new"
    let target = "examples" </> problem </> "problem.expression"
    goldenVsFile "get problem expression" target source (pure ())
 where
  problem = problemPath name number

getMap :: Natural -> Writer ([TestTree] -> [TestTree]) ()
getMap number = write do
  let source = "examples" </> "lambdaman" </> show number </> "problem.expression"
  let target = "examples" </> "lambdaman" </> show number </> "problem.string"
  goldenVsString "get map" target do
    expression ← getExpressionFromFile source
    case evalExpr emptyEnvironment expression of
      Left errorMessage → throwIO do CannotEvaluate expression errorMessage
      Right (EStr text) → (pure . Bytes.fromStrict . Text.encodeUtf8) text
      Right otherExpression → throwIO do ExpectedString otherExpression


getSolutionExpression :: String -> Natural -> (Expr -> Either String Expr) -> Writer ([TestTree] -> [TestTree]) ()
getSolutionExpression name number solve =
  write do
    let source = "examples" </> problem </> "problem.expression"
    let target = "examples" </> problem </> "solution.expression"
    goldenVsString
      "get solution expression"
      target
      do
        problemExpression <- getExpressionFromFile source
        solutionExpression <- case solve problemExpression of
          Left errorMessage -> throwIO do CannotSolveProblem errorMessage
          Right solution -> pure solution
        pure do expressionToBytes solutionExpression
 where
  problem = problemPath name number

communicateSolution :: String -> Natural -> Writer ([TestTree] -> [TestTree]) ()
communicateSolution name number =
  checkCommunication
    problem
    "communicate solution"
    do
      flip
        fmap
        do getExpressionFromFile ("examples" </> problem </> "solution.expression")
        \case
          EStr text -> Codegen.pack (Text.unwords ["solve", Text.pack name <> (Text.pack . show) number, text])
          _ -> error "Not implemented."
 where
  problem = problemPath name number

checkCorrectness :: String -> Natural -> Writer ([TestTree] -> [TestTree]) ()
checkCorrectness name number = (writeProperty "check correctness" . ioProperty) do
  response <- getExpressionFromFile ("examples" </> problem </> "communicate solution" </> "response.expression")
  wordsOfResponse <- case evalExpr emptyEnvironment response of
    Right (EStr text) -> pure do Text.words text
    _ -> throwIO do ResponseIsNotString response
  pure do wordsOfResponse Partial.!! 0 === "Correct,"
 where
  problem = problemPath name number

checkCost :: String -> Natural -> Writer ([TestTree] -> [TestTree]) ()
checkCost name number = (writeProperty "check cost" . ioProperty) do
  response <- getExpressionFromFile ("examples" </> problem </> "communicate solution" </> "response.expression")
  wordsOfResponse <- case evalExpr emptyEnvironment response of
    Right (EStr text) -> pure do Text.words text
    _ -> throwIO do ResponseIsNotString response
  currentCost <- readIO @Natural (PartialText.init (wordsOfResponse Partial.!! 8))
  lastCostText <-
    Text.readFile ("examples" </> problem </> "cost.number")
      `catch` \exception ->
        if doesNotExistErrorType == ioeGetErrorType exception
          then do
            let newText = (Text.pack . show) currentCost
            Text.writeFile ("examples" </> problem </> "cost.number") newText
            pure newText
          else throwIO exception
  lastCost <- readIO @Natural lastCostText
  if currentCost > lastCost
    then throwIO do CostIncreased currentCost lastCost
    else do
      let newText = (Text.pack . show) currentCost
      Text.writeFile ("examples" </> problem </> "cost.number") newText
      pure True
 where
  problem = problemPath name number

readIO :: (Read read) => Text -> IO read
readIO text = case (readMaybe . Text.unpack) text of
  Nothing -> throwIO do CannotRead text
  Just result -> pure result

getExpressionFromFile :: FilePath -> IO Expr
getExpressionFromFile = Text.readFile >=> readIO

expressionToBytes :: Expr -> Bytes.ByteString
expressionToBytes = Bytes.fromStrict . Text.encodeUtf8 . Text.pack . show

decodeExpressionFromFile :: FilePath -> IO Expr
decodeExpressionFromFile filePath = do
  expressionBytes <- Bytes.readFile filePath
  expression <- case (parseExpr . Text.decodeUtf8With lenientDecode . Bytes.toStrict) expressionBytes of
    Right parsedExpression -> pure parsedExpression
    Left errorMessage -> throwIO do CannotDecodeExpression errorMessage
  pure expression

encodeExpressionToBytes :: Expr -> Bytes.ByteString
encodeExpressionToBytes = Bytes.fromStrict . Text.encodeUtf8 . Printer.print

data CheckingException
  = CannotDecodeExpression String
  | CannotSolveProblem String
  | ResponseIsNotString Expr
  | CannotRead Text
  | CostIncreased Natural Natural
  | CannotEvaluate Expr String
  | ExpectedString Expr
  deriving (Show)
instance Exception CheckingException

newtype AppropriateText = AppropriateText Text deriving newtype (Show, Eq, Ord)
instance Arbitrary AppropriateText where
  arbitrary = do
    text <- arbitrary
    pure do AppropriateText (Text.filter (`elem` charOrder) text)
  shrink = coerce (shrink :: Text -> [Text])

newtype AppropriateEncodedText = AppropriateEncodedText Text deriving newtype (Show, Eq, Ord)
instance Arbitrary AppropriateEncodedText where
  arbitrary = do
    text <- arbitrary
    pure do AppropriateEncodedText (Text.filter (\character -> 33 <= ord character && ord character <= 126) text)
  shrink = coerce (shrink :: Text -> [Text])

checkCommunication :: String -> String -> IO Expr -> Writer ([TestTree] -> [TestTree]) ()
checkCommunication problem task getExpression = sequentialTestWriter task do
  write do
    goldenVsString
      "input"
      (rootPath </> "request.expression")
      do fmap expressionToBytes getExpression
  write do
    goldenVsString
      "encoding"
      (rootPath </> "request.bytes")
      do fmap encodeExpressionToBytes getExpression
  write do
    goldenVsString
      "fetching"
      (rootPath </> "response.bytes")
      do
        requestBytes <- Bytes.readFile (rootPath </> "request.bytes")
        postBytes requestBytes
  write do
    goldenVsFile
      "decoding"
      (rootPath </> "response.expression")
      (rootPath </> "response.expression.new")
      do
        responseExpression <- decodeExpressionFromFile (rootPath </> "response.bytes")
        Bytes.writeFile (rootPath </> "response.expression.new") (expressionToBytes responseExpression)
 where
  rootPath = "examples" </> problem </> task

writerMain :: Writer ([TestTree] -> [TestTree]) () -> IO ()
writerMain = defaultMain . testGroup "main" . ($ []) . execWriter

testWriter :: TestName -> Writer ([TestTree] -> [TestTree]) () -> Writer ([TestTree] -> [TestTree]) ()
testWriter name = write . testGroup name . ($ []) . execWriter

sequentialTestWriter :: TestName -> Writer ([TestTree] -> [TestTree]) () -> Writer ([TestTree] -> [TestTree]) ()
sequentialTestWriter name = write . sequentialTestGroup name AllSucceed . ($ []) . execWriter

writeProperty :: (Testable testable) => TestName -> testable -> Writer ([TestTree] -> [TestTree]) ()
writeProperty = fmap write . testProperty

write :: (Monad monad) => anything -> WriterT ([anything] -> [anything]) monad ()
write item = tell (++ [item])
