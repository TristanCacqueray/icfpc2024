module ProgCon.API where

import RIO
import RIO.ByteString (toStrict)
import RIO.ByteString.Lazy qualified as Lazy
import RIO.Text qualified as Text

import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment

import Network.HTTP.Types (Status (..))

apiServer :: String
apiServer = "https://boundvariable.space/communicate"

postBytes :: Lazy.ByteString -> IO Lazy.ByteString
postBytes body = do
  manager <- newTlsManager
  token <- getEnv "ICFP_TOKEN"
  debug <- lookupEnv "ICFP_DEBUG"
  initialRequest <- parseRequest apiServer
  let request =
        initialRequest
          { method = "POST"
          , requestBody = RequestBodyLBS $ body
          , requestHeaders =
              [ ("Authorization", "Bearer " <> encodeUtf8 (Text.pack token))
              ]
          }
  response <- httpLbs request manager
  when (isJust debug) do
    putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  pure $ responseBody response

communicate :: Text -> IO Text
communicate message = do
  response <- (postBytes . Lazy.fromStrict . encodeUtf8) message
  (pure . decodeUtf8 . toStrict) response
