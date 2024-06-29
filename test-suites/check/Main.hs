module Main where

import RIO
import RIO.ByteString.Lazy qualified as Bytes
import RIO.Char
import RIO.Text qualified as Text

import Control.Monad.Trans.Writer.CPS
import Data.Coerce
import Data.Text.IO qualified as Text
import System.FilePath
import Test.QuickCheck.Instances.Natural ()
import Test.QuickCheck.Instances.Text ()
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.QuickCheck

import ProgCon.API
import ProgCon.Eval
import ProgCon.Parser
import ProgCon.Printer qualified as Printer
import Spaceship qualified

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

slowChecks :: Writer ([TestTree] -> [TestTree]) ()
slowChecks = do
  testWriter "solutions" do
    forM_ problems checkProblem

checkProblem :: ProblemDefinition -> Writer ([TestTree] -> [TestTree]) ()
checkProblem ProblemDefinition {..} = testWriter name do
  forM_ [1 .. size] \number ->
    let problem = "spaceship" </> show number
    in  sequentialTestWriter (show number) do
          checkCommunication
            problem
            "communicate problem"
            do pure do EStr (Text.unwords ["get", "spaceship" <> (Text.pack . show) number])

          write do
            let source = "examples" </> problem </> "communicate problem" </> "response.expression.new"
            let target = "examples" </> problem </> "problem.expression"
            goldenVsFile "get problem expression" target source (pure ())

          write do
            let source = "examples" </> problem </> "problem.expression"
            let target = "examples" </> problem </> "solution.expression"
            goldenVsString
              "get solution expression"
              target
              do
                problemExpressionText <- Text.readFile source
                do
                  problemExpression <- case (readMaybe @Expr . Text.unpack) problemExpressionText of
                    Nothing -> error "Problem is encoded incorrectly!"
                    Just expression -> pure expression
                  solutionExpression <- case solve problemExpression of
                    Left errorMessage -> error errorMessage
                    Right solution -> pure solution
                  pure do (Bytes.fromStrict . Text.encodeUtf8 . Text.pack . show) solutionExpression

          checkCommunication
            problem
            "communicate solution"
            do
              flip
                fmap
                do Text.readFile ("examples" </> problem </> "solution.expression")
                \solutionExpressionText ->
                  case (readMaybe @Expr . Text.unpack) solutionExpressionText of
                    Nothing -> error "Solution is encoded incorrectly!"
                    Just (EStr text) -> EStr (Text.unwords ["solve", "spaceship" <> (Text.pack . show) number, text])
                    Just _ -> error "Not implemented."

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
checkCommunication problem task getExpression =
  let rootPath = "examples" </> problem </> task
      getEncodedExpression = fmap Printer.print getExpression
  in  sequentialTestWriter task do
        write do
          goldenVsString
            "input"
            (rootPath </> "request.expression")
            do fmap (Bytes.fromStrict . Text.encodeUtf8 . Text.pack . show) getExpression
        write do
          goldenVsString
            "encoding"
            (rootPath </> "request.bytes")
            do fmap (Bytes.fromStrict . Text.encodeUtf8) getEncodedExpression
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
              responseBytes <- Bytes.readFile (rootPath </> "response.bytes")
              output <- case (parseExpr . Text.decodeUtf8With lenientDecode . Bytes.toStrict) responseBytes of
                Right parsedExpression -> (pure . Bytes.fromStrict . Text.encodeUtf8 . Text.pack . show) parsedExpression
                Left errorMessage -> error errorMessage
              Bytes.writeFile (rootPath </> "response.expression.new") output

checkQueryString :: TestName -> TestTree
checkQueryString path = goldenVsString
  path
  ("examples" </> "queryString" </> path <.> "text")
  do
    fmap (Bytes.fromStrict . Text.encodeUtf8) (queryString (Text.pack path))

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
