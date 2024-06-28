module Main where

import RIO
import RIO.ByteString.Lazy qualified as Bytes
import RIO.Char
import RIO.Text qualified as Text

import Control.Monad.Trans.Writer.CPS
import Data.Coerce
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

main :: IO ()
main = writerMain do
  testWriter "fast checks" do
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

  testWriter "slow checks" do
    testWriter "query strings" do
      write do checkQueryString "get index"
      forM_ [1 :: Int .. 25] \number -> write do checkQueryString ("get spaceship" <> show number)
      forM_ [1 :: Int .. 5] \number -> write do checkQueryString ("get lambdaman" <> show number {- TODO there are also 6 to 21, but they hang the evaluator -})
    testWriter "communicate" do
      checkCommunication (EStr "get index")
      forM_ [1 :: Int .. 25] \number -> checkCommunication (EStr ("get spaceship" <> (Text.pack . show) number))
      forM_ [1 :: Int .. 21] \number -> checkCommunication (EStr ("get lambdaman" <> (Text.pack . show) number))

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

checkCommunication :: Expr -> Writer ([TestTree] -> [TestTree]) ()
checkCommunication expression =
  let expressionText = Printer.print expression
      expressionString = show expression
      rootPath = "examples" </> "communicate" </> expressionString
  in  sequentialTestWriter expressionString do
        write do
          goldenVsString
            "encoding"
            (rootPath </> "request.bytes")
            do (pure . Bytes.fromStrict . Text.encodeUtf8) expressionText
        write do
          goldenVsString
            "fetching"
            (rootPath </> "response.bytes")
            do
              requestBytes <- Bytes.readFile (rootPath </> "request.bytes")
              postBytes requestBytes
        write do
          goldenVsString
            "decoding"
            (rootPath </> "response.expression")
            do
              responseBytes <- Bytes.readFile (rootPath </> "response.bytes")
              case (parseExpr . Text.decodeUtf8With lenientDecode . Bytes.toStrict) responseBytes of
                Right parsedExpression -> (pure . Bytes.fromStrict . Text.encodeUtf8 . Text.pack . show) parsedExpression
                Left errorMessage -> error errorMessage

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
