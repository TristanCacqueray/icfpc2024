module ProgCon (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import RIO
import SimpleCmdArgs
import Text.Pretty.Simple qualified as Pretty

import ProgCon.API
import ProgCon.API qualified as API
import ProgCon.Eval qualified as Eval
import ProgCon.Parser
import ProgCon.Parser qualified as Parser
import RIO.Directory (createDirectoryIfMissing, doesFileExist)
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
      , Subcommand "parse-file" "parse a message" $
          mainParseFile <$> (strArg "FP")
      , Subcommand "eval" "eval a message" $
          mainEval <$> (T.pack <$> strArg "MESSAGE")
      , Subcommand "solve-spaceship" "solve a spaceship puzzle" $
          solveSpaceship <$> argumentWith auto "NUM"
      , Subcommand "sync-puzzles" "fetch all the puzzles" $
          pure mainSync
      ]

mainSync :: IO ()
mainSync =
  traverse_
    syncCourse
    [ -- ("lambdaman", 21),
      ("spaceship", 25)
    ]
 where
  syncCourse (name, count) = do
    createDirectoryIfMissing True dir
    traverse_ syncPuzzle [1 :: Int .. count]
   where
    dir = "courses/" <> name <> "/"
    syncPuzzle nr = do
      let fp = dir <> show nr <> ".txt"
      doesFileExist fp >>= \case
        True -> pure ()
        False -> do
          putStrLn $ "Syncing " <> fp
          handleQuery (T.pack $ "get " <> name <> show nr) >>= \case
            Left e -> T.putStrLn "Bad resp:" >> Pretty.pPrint e
            Right txt -> T.writeFile fp txt

solveSpaceship :: Int -> IO ()
solveSpaceship nr = do
  let fp = "courses/spaceship/" <> show nr <> ".txt"
  courseInput <- T.readFile fp
  putStrLn $ Spaceship.solve $ Spaceship.parseInput courseInput

mainCommunicate :: Text -> IO ()
mainCommunicate message = do
  T.putStrLn =<< API.communicate message

mainParseFile :: FilePath -> IO ()
mainParseFile fp = mainParse . T.strip =<< T.readFile fp

mainParse :: Text -> IO ()
mainParse message = case Parser.parseExpr message of
  Left err -> do
    T.putStrLn message
    error err
  Right expr -> case expr of
    Parser.EStr txt -> T.putStrLn txt
    _ -> Pretty.pPrint expr

mainEncode :: Text -> IO ()
mainEncode message = T.putStrLn (Parser.encodeString message)

handleQuery :: Text -> IO (Either Expr Text)
handleQuery message = do
  let expression = case (readMaybe @Expr . T.unpack) message of
        Nothing -> EStr message
        Just expression' -> expression'
  responseExpression <- query expression
  pure $ case responseExpression of
    EStr text -> Right text
    _ -> Left responseExpression

mainQuery :: Text -> IO ()
mainQuery message =
  handleQuery message >>= \case
    Right text -> T.putStrLn text
    otherResponseExpression -> Pretty.pPrint otherResponseExpression

mainEval :: Text -> IO ()
mainEval message = case Parser.parseExpr message of
  Left err -> error err
  Right expr -> case Eval.evalExpr mempty expr of
    Right (Parser.EStr txt) -> T.putStrLn (Parser.decodeString txt)
    res -> Pretty.pPrint res
