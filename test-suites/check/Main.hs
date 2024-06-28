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

main :: IO ()
main = writerMain do
  testWriter "checks" do
    testWriter "communicate" do
      write do
        checkCommunicationBytes "get index"
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

checkCommunicationBytes :: TestName -> TestTree
checkCommunicationBytes path = goldenVsString
  path
  ("examples" </> "communicate" </> path </> "response.bytes")
  do
    requestBytes <- Bytes.readFile ("examples" </> "communicate" </> path </> "request.bytes")
    postBytes requestBytes

writerMain :: Writer ([TestTree] -> [TestTree]) () -> IO ()
writerMain = defaultMain . testGroup "main" . ($ []) . execWriter

testWriter :: TestName -> Writer ([TestTree] -> [TestTree]) () -> Writer ([TestTree] -> [TestTree]) ()
testWriter name = write . testGroup name . ($ []) . execWriter

writeProperty :: (Testable testable) => TestName -> testable -> Writer ([TestTree] -> [TestTree]) ()
writeProperty = fmap write . testProperty

write :: (Monad monad) => anything -> WriterT ([anything] -> [anything]) monad ()
write item = tell (++ [item])
