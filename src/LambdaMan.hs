{-# LANGUAGE NoImplicitPrelude #-}

module LambdaMan where

import RIO
import RIO.List qualified as List
import Prelude qualified

import LambdaMan.Pictures
import LambdaMan.Types
import ProgCon.Parser

import Data.Array hiding (array, index)
import Data.Array.MArray hiding (index)
import Data.Array.ST.Safe hiding (index)
import Graphics.Gloss qualified as Gloss
import Graphics.Gloss.Interface.Pure.Simulate qualified as Gloss
import Linear
import RIO.List.Partial qualified as Partial

solveExpression :: Expr -> Either String Expr
solveExpression = (const . Right) (EStr "LLLDURRRUDRRURR")

validateExpression :: Expr -> Either String Float
validateExpression = const (Left "Not implemented.")

readBoard :: FilePath -> IO Board
readBoard path = do
  array <- (fmap makeBoardArray . Prelude.readFile) path
  let me = (fst . fromMaybe (error "There should be a Lambda Man somewhere!") . List.find (\(_, spot) -> spot == Me) . assocs) array
  pure Board {..}

makeBoardArray :: String -> Array (V2 Int) Spot
makeBoardArray string = runSTArray do
  board <- newArray (V2 0 0, V2 (width - 1) (height - 1)) Floor
  forM_ (rows `zip` [0 ..]) \(row, j) ->
    forM_ (row `zip` [0 ..]) \(character, i) ->
      writeArray board (V2 i j) (meaning character)
  pure board
 where
  rows = lines string
  height = List.genericLength rows
  width = Partial.maximum (0 :| fmap List.genericLength rows)

meaning :: Char -> Spot
meaning = \case
  '.' -> Floor
  '#' -> Wall
  'L' -> Me
  character -> error do "Meaningless character " <> show character <> "!"

drawBoard :: Natural -> IO ()
drawBoard level = do
  board <- readBoard ("examples/lambdaman/" <> show level <> "/problem.string")
  Gloss.simulate Gloss.FullScreen backgroundColour 4 (markLegalSteps board) pictureOfBoard animateBoard

animateBoard :: Gloss.ViewPort -> Float -> Board -> Board
animateBoard _ _ board = case legalSteps board of
  [] -> board
  (direction : _) -> (markLegalSteps . fromMaybe (error "Legal step failed!") . flip makeStep direction . unmarkLegalSteps) board

vectorOfDirection :: Direction -> V2 Int
vectorOfDirection U = V2 0 (-1)
vectorOfDirection R = V2 1 0
vectorOfDirection D = V2 0 1
vectorOfDirection L = V2 (-1) 0

(?) :: (Ix index) => Array index value -> index -> Maybe value
array ? index
  | inRange (bounds array) index = Just do array ! index
  | otherwise = Nothing
infixl 9 ?

canStepOnThis :: Spot -> Bool
canStepOnThis = \case
  Floor -> True
  LegalStep -> True
  _ -> False

makeStep :: Board -> Direction -> Maybe Board
makeStep Board {..} direction =
  let newMe = (me + vectorOfDirection direction)
  in  case array ? newMe of
        Just tile
          | canStepOnThis tile ->
              Just
                Board
                  { me = newMe
                  , array = runSTArray do
                      mutableArray <- thaw array
                      writeArray mutableArray me FootStep
                      writeArray mutableArray newMe Me
                      pure mutableArray
                  }
        _ -> Nothing

legalSteps :: Board -> [Direction]
legalSteps board = filter (isJust . makeStep board) [minBound .. maxBound]

markLegalSteps :: Board -> Board
markLegalSteps board@Board {..} =
  board
    { array = runSTArray do
        mutableArray <- thaw array
        forM_ (legalSteps board) \direction -> writeArray mutableArray (me + vectorOfDirection direction) LegalStep
        pure mutableArray
    }

unmarkLegalSteps :: Board -> Board
unmarkLegalSteps board@Board {..} =
  board
    { array = runSTArray do
        mutableArray <- thaw array
        forM_ (legalSteps board) \direction -> writeArray mutableArray (me + vectorOfDirection direction) Floor
        pure mutableArray
    }
