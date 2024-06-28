module Main where

import RIO
import RIO.ByteString.Lazy qualified as Bytes

import Control.Monad.Trans.Writer.CPS
import System.FilePath
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
    testWriter "evaluator" do
      writeProperty "hello" do
        evalExpr
          emptyEnvironment
          (EBinary '$' (EBinary '$' (ELam 2 (ELam 3 (EVar 2))) (EBinary '.' (EStr "Hello") (EStr " World!"))) (EInt 42))
          === Right (EStr "Hello World!")

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
