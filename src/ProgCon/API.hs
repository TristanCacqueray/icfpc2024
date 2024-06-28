module ProgCon.API where

import RIO
import RIO.ByteString (toStrict)
import RIO.ByteString.Lazy qualified as Lazy
import RIO.FilePath
import RIO.List qualified as List
import RIO.Text qualified as Text

import Data.Text.Encoding (decodeUtf8)
import Data.Time
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types (Status (..))
import System.Environment
import System.IO.Error
import System.IO.Temp

import ProgCon.Parser
import ProgCon.Printer qualified as Printer

apiServer :: String
apiServer = "https://boundvariable.space/communicate"

rateLimited :: IO anything -> IO anything
rateLimited action = do
  debug <- lookupEnv "ICFP_DEBUG"
  temporaryDirectory <- getCanonicalTemporaryDirectory
  let timersFile = temporaryDirectory </> "icfp-timers"
  currentTime <- getCurrentTime
  timers <- fmap (fmap (read @UTCTime) . lines) do
    readFile timersFile
      `catch` \exception -> if doesNotExistErrorType == ioeGetErrorType exception then pure [] else throwIO exception
  let (expiredTimers, runningTimers) = List.partition (\timer -> currentTime `diffUTCTime` timer > 60) timers
  if length runningTimers > 18
    then do
      when (isJust debug) do
        (putStrLn . unwords) ["expired timers", show expiredTimers, "waiting for timers", show runningTimers]
      threadDelay 1000000
      rateLimited action
    else do
      writeFile timersFile ((unlines . fmap show) (currentTime : runningTimers))
      action

postBytes :: Lazy.ByteString -> IO Lazy.ByteString
postBytes body = rateLimited do
  debug <- lookupEnv "ICFP_DEBUG"
  manager <- newTlsManager
  token <- getEnv "ICFP_TOKEN"
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

data ApiException = ParseError String deriving (Show)
instance Exception ApiException

query :: Expr -> IO Expr
query = (=<<) (either (throwIO . ParseError) pure) . fmap parseExpr . communicate . Printer.print
