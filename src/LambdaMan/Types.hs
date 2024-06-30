module LambdaMan.Types where

import Data.Array
import Linear

data Direction = U | R | D | L deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Spot = Me | FootStep | LegalStep | Wall | Floor deriving (Show, Read, Eq, Ord)

data Board = Board
  { array :: Array (V2 Int) Spot
  , me :: V2 Int
  }
  deriving (Show, Read, Eq, Ord)
