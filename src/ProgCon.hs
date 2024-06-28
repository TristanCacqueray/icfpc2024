module ProgCon (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import RIO
import SimpleCmdArgs
import Text.Pretty.Simple qualified as Pretty

import ProgCon.API qualified as API
import ProgCon.Eval qualified as Eval
import ProgCon.Parser qualified as Parser
import Spaceship qualified

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  mainMain

mainMain :: IO ()
mainMain =
  simpleCmdArgs Nothing "progcon" "musical concert" $
    subcommands
      [ Subcommand "communicate" "send a message" $
          mainCommunicate
            <$> (T.pack <$> strArg "MESSAGE")
      , Subcommand "query" "encode and send a string message" $
          mainQuery
            <$> (T.pack <$> strArg "MESSAGE")
      , Subcommand "encode" "encode a message" $
          mainEncode
            <$> (T.pack <$> strArg "STRING")
      , Subcommand "parse" "parse a message" $
          mainParse
            <$> (T.pack <$> strArg "MESSAGE")
      , Subcommand "eval" "eval a message" $
          mainEval <$> (T.pack <$> strArg "MESSAGE")
      , Subcommand "solve-spaceship" "solve a spaceship puzzle" $
          solveSpaceship <$> argumentWith auto "NUM"
      ]

solveSpaceship :: Int -> IO ()
solveSpaceship nr = do
  let fp = "courses/spaceship/" <> show nr <> ".txt"
  courseInput <- T.readFile fp
  print courseInput
  mapM_ print $ Spaceship.parseInput courseInput

mainCommunicate :: Text -> IO ()
mainCommunicate message = do
  T.putStrLn =<< API.communicate message

mainParse :: Text -> IO ()
mainParse message = case Parser.parseExpr message of
  Left err -> do
    T.putStrLn message
    error err
  Right expr -> case expr of
    Parser.EStr txt -> T.putStrLn (Parser.decodeString txt)
    _ -> Pretty.pPrint expr

mainEncode :: Text -> IO ()
mainEncode message = T.putStrLn (Parser.encodeString message)

mainQuery :: Text -> IO ()
mainQuery message = do
  mainParse =<< API.communicate (Parser.encodeString message)

mainEval :: Text -> IO ()
mainEval message = case Parser.parseExpr message of
  Left err -> error err
  Right expr -> case Eval.evalExpr mempty expr of
    Right (Parser.EStr txt) -> T.putStrLn (Parser.decodeString txt)
    res -> Pretty.pPrint res
