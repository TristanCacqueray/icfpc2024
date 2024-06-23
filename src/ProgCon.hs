module ProgCon (main) where

import RIO
import Control.Concurrent (forkIO)
import Data.List.Extra (groupSortOn)
import Data.Vector.Unboxed qualified as UV
import Say
import SimpleCmdArgs
import System.Directory (doesFileExist)

import ProgCon.API (scoreBoard, userBoard)
import ProgCon.Eval
import ProgCon.GUI
import ProgCon.Parser
import ProgCon.Solve
import ProgCon.Syntax
import ProgCon.Submit
import ProgCon.Utils
import Data.List (sortOn)
import Text.Printf (printf)
import Data.Time (UTCTime, nominalDiffTimeToSeconds)
import RIO.Directory (getModificationTime)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Control.Scheduler (Comp(Par), withScheduler_, scheduleWork_)

-- | Change the set of problem globally by tweaking 'allProblems'
allProblems :: [ProblemID]
allProblems = filter (/= 38) [1..90]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  mainMain

mainMain :: IO ()
mainMain =
  simpleCmdArgs Nothing "progcon" "musical concert" $
  subcommands
  [ Subcommand "solve" "solve problem" $
    mainSolver
    <$> switchWith 'N' "no-load" "ignore the existing solution"
    <*> some intArg
  , Subcommand "submit" "submit problem solution" $
    submitOne False
    <$> switchWith 'r' "retry" "retry for network issues"
    <*> intArg
  ]
  where
    intArg :: Parser ProblemID
    intArg = ProblemID <$> argumentWith auto "NUM"

mainSolver :: Bool -> [ProblemID] -> IO ()
mainSolver ignoreSoln pids = undefined
