{-# LANGUAGE PatternSynonyms #-}

module ProgCon.Syntax where

import Data.Aeson (ToJSON(..), Value(Null))
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed qualified as UV
import Text.Printf (printf)

newtype ProblemID = ProblemID Int
    deriving newtype (Show, Eq, Ord, Enum, Num, ToJSON)

pattern SpecProblem :: ProblemID
pattern SpecProblem = ProblemID 0

problemBase :: ProblemID -> FilePath
problemBase SpecProblem = "problems/spec"
problemBase (ProblemID pid) = "problems/" <> printf "%02d" pid

problemPath :: ProblemID -> FilePath
problemPath pid = problemBase pid <> "-problem.json"

solutionPath :: ProblemID -> FilePath
solutionPath pid = problemBase pid <> "-solution.json"

data Params = Params
    deriving (Show, Eq, Ord)

data Problem = Problem
    deriving (Show, Eq, Ord)

-- | This is the final solution representation to be submitted
data Solution = Solution
    deriving (Show, Eq, Ord)
