module LambdaMan.Types where

import RIO
import RIO.ByteString qualified as ByteArray
import RIO.Partial qualified as Partial

import Control.Comonad.Cofree
import Data.Array
import Linear

data Direction = U | R | D | L deriving (Show, Read, Eq, Ord, Enum, Bounded)

reverseDirection :: Direction -> Direction
reverseDirection = \case
  U -> D
  R -> L
  D -> U
  L -> R

word8OfDirection :: Direction -> Word8
word8OfDirection = fromIntegral . fromEnum
directionOfWord8 :: Word8 -> Direction
directionOfWord8 = Partial.toEnum . fromIntegral

vectorOfDirection :: Direction -> V2 Int
vectorOfDirection = \case
  U -> V2 0 (-1)
  R -> V2 1 0
  D -> V2 0 1
  L -> V2 (-1) 0

data Spot = Me | FootStep | LegalStep | Wall | Floor deriving (Show, Read, Eq, Ord)

data Board = Board
  { array :: Array (V2 Int) Spot
  , me :: V2 Int
  }
  deriving (Show, Read, Eq, Ord)

data AnimationState = AnimationState
  {board :: Board, solution :: ByteArray, spanningTree :: Cofree (Map Direction) (V2 Int)}

type ByteArray = ByteArray.ByteString
