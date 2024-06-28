module ProgCon (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import ProgCon.API qualified as API
import ProgCon.Parser qualified as Parser
import RIO
import SimpleCmdArgs

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
            ]

mainCommunicate :: Text -> IO ()
mainCommunicate message = do
    T.putStrLn =<< API.communicate message

mainParse :: Text -> IO ()
mainParse message = case Parser.parseExpr message of
    Left err -> error err
    Right expr -> case expr of
        Parser.EStr txt -> T.putStrLn txt
        _ -> print expr

mainEncode :: Text -> IO ()
mainEncode message = T.putStrLn (Parser.encodeString message)

mainQuery :: Text -> IO ()
mainQuery message = do
    mainParse =<< API.communicate (Parser.encodeString message)
