module ProgCon.API where

import Data.ByteString (fromStrict, toStrict)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment

import Network.HTTP.Types (Status (..))

apiServer :: String
apiServer = "https://boundvariable.space/communicate"

communicate :: Text -> IO Text
communicate body = do
    manager <- newTlsManager
    token <- getEnv "ICFP_TOKEN"
    initialRequest <- parseRequest apiServer
    let request =
            initialRequest
                { method = "POST"
                , requestBody = RequestBodyLBS $ fromStrict $ encodeUtf8 body
                , requestHeaders =
                    [ ("Authorization", "Bearer " <> encodeUtf8 (pack token))
                    ]
                }
    response <- httpLbs request manager
    putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
    pure $ decodeUtf8 (toStrict $ responseBody response)
