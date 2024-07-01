module ProgCon (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy as T (toStrict)
import Graphics.Gloss qualified as Gloss
import RIO
import SimpleCmdArgs
import Text.Pretty.Simple qualified as Pretty

import LambdaMan qualified
import ProgCon.API
import ProgCon.API qualified as API
import ProgCon.Eval qualified as Eval
import ProgCon.Parser
import ProgCon.Parser qualified as Parser
import ProgCon.Printer qualified as Printer
import RIO.Directory (createDirectoryIfMissing, doesFileExist)
import SimpleCmd.Git qualified
import Spaceship qualified
import Spaceship.Pictures qualified

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
      , Subcommand "parse-file" "parse a message" $ mainParseFile <$> strArg "FP"
      , Subcommand "eval" "eval a message" $
          mainEval <$> (T.pack <$> strArg "MESSAGE")
      , Subcommand "eval-file" "eval a message" $ mainEvalFile <$> strArg "FP"
      , Subcommand "solve-spaceship" "solve a spaceship puzzle" $
          solveSpaceship <$> argumentWith auto "NUM"
      , Subcommand "draw-spaceship" "draw a spaceship puzzle" $
          drawSpaceship <$> argumentWith auto "NUM"
      , Subcommand "pull-puzzles" "fetch all the puzzles" $
          pure mainPull
      , Subcommand "push-solutions" "submit all new solutions" $ pure mainPush
      , Subcommand "lambdaman-draw-map" "draw a map for LambdaMan" $ pure LambdaMan.drawBoard <*> argumentWith auto "LEVEL"
      ]

-- | Submit any modified files in `courses/%s/%d.bytes` and stage them with `git add`.
-- Run this command after creating new solutions.
mainPush :: IO ()
mainPush =
  SimpleCmd.Git.git "status" ["--porcelain"] >>= \(lines -> xs) -> do
    traverse_ pushSolution xs
 where
  pushSolution (_ : status : _ : path)
    | "courses/" `T.isPrefixOf` T.pack path && ".bytes" `T.isSuffixOf` T.pack path = case status of
        '?' -> doPush path
        'M' -> doPush path
        ' ' -> pure ()
        _ -> putStrLn $ "Ignoring path: " <> show status <> " " <> show path
  pushSolution _ = pure ()
  doPush fp = do
    putStrLn $ "[+] Submiting " <> fp
    bytes <- T.strip <$> T.readFile fp
    resp <- API.communicate bytes
    case parseExpr resp of
      Right (Parser.EStr txt) -> do
        T.putStrLn $ "OK: " <> txt
        SimpleCmd.Git.git_ "add" [fp]
      expr -> Pretty.pPrint expr
    -- exitSuccess
    pure ()

mainPull :: IO ()
mainPull = do
  traverse_
    syncCourse
    [ ("lambdaman", 21)
    , ("spaceship", 25)
    , ("3d", 12)
    , ("efficiency", 13)
    ]
 where
  syncCourse (name, count) = do
    createDirectoryIfMissing True dir
    traverse_ syncPuzzle [1 :: Int .. count]
   where
    dir = "courses/" <> name <> "/"
    syncPuzzle nr = do
      let fp = dir <> show nr <> ".expr"
      doesFileExist fp >>= \case
        True -> pure ()
        False -> do
          putStrLn $ "Syncing " <> fp <> ", query \"get " <> name <> show nr <> "\""
          txt <- rawQuery (T.pack $ "get " <> name <> show nr)
          T.writeFile fp txt


solveSpaceship :: Int -> IO ()
solveSpaceship nr = do
  let ifp = "courses/spaceship/" <> show nr <> ".txt"
  courseInput <- T.readFile ifp
  let targets = Spaceship.parseInput courseInput
  putStrLn $ "Solving " <> ifp <> ": " <> show (length targets)
  let ordered = Spaceship.optimizeOrder targets
  -- putStrLn $ "Optimized " <> ifp <> ": " <> show (take 10 targets)
  let thrusts = Spaceship.solve ordered
  let ofp = "courses/spaceship/" <> show nr <> ".bytes"
  putStrLn $ "Done: " <> show (length thrusts)
  T.writeFile ofp $ Printer.print $ EStr $ "solve spaceship" <> T.pack (show nr <> " " <> thrusts)

drawSpaceship :: Int -> IO ()
drawSpaceship nr = do
  let ifp = "courses/spaceship/" <> show nr <> ".txt"
  courseInput <- T.readFile ifp
  let targets = Spaceship.parseInput courseInput
  putStrLn $ "Solving " <> ifp <> ": " <> show (length targets)
  let ordered = Spaceship.optimizeOrder targets
  let model = Spaceship.Pictures.newModel (Spaceship.solve ordered, ordered)
  Gloss.simulate glossWin Gloss.white 3 model Spaceship.Pictures.drawModel (const Spaceship.Pictures.stepModel)

glossWin :: Gloss.Display
glossWin = Gloss.InWindow "icfp2024" (1024, 1080) (10, 10)

mainCommunicate :: Text -> IO ()
mainCommunicate message = do
  T.putStrLn =<< API.communicate message

mainParseFile :: FilePath -> IO ()
mainParseFile fp = do
  txt <- T.strip <$> T.readFile fp
  print txt
  mainParse txt

mainParse :: Text -> IO ()
mainParse message = case Parser.parseExpr message of
  Left err -> do
    T.putStrLn message
    error err
  Right expr -> case expr of
    Parser.EStr txt -> T.putStrLn $ "OK: " <> txt
    _ -> Pretty.pPrint expr

mainEncode :: Text -> IO ()
mainEncode message = T.putStrLn (Parser.encodeString message)

rawQuery :: Text -> IO Text
rawQuery message = API.communicate $ Printer.print $ EStr message

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
    Left expr -> T.putStrLn $ Printer.print expr

mainEvalFile :: FilePath -> IO ()
mainEvalFile fp = mainEval =<< fmap T.strip (T.readFile fp)

mainEval :: Text -> IO ()
mainEval message = case Parser.parseExpr message of
  Left err -> error err
  Right expr -> case Eval.evalExpr expr of
    Right (Parser.EStr txt) -> T.putStrLn txt
    res -> Pretty.pPrint res
