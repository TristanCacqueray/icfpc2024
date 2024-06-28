module ProgCon (main) where

import Data.Text qualified as T
import RIO
import SimpleCmdArgs
import ProgCon.API qualified as API
import ProgCon.Parser qualified as Parser

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
    <$> (T.pack <$> strArg "MESSAGE"),
    Subcommand "parse" "parse a message" $
    mainParse
    <$> (T.pack <$> strArg "MESSAGE")

  ]

mainCommunicate :: Text -> IO ()
mainCommunicate message = do
  print =<< API.communicate message

mainParse :: Text -> IO ()
mainParse message = do
  print $ Parser.parseExpr message
