{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoPartialTypeSignatures #-}
{-# OPTIONS_GHC -Wpartial-type-signatures #-}

module LambdaMan where

import RIO hiding (fold)
import RIO.ByteString qualified as ByteArray
import RIO.List qualified as List
import RIO.List.Partial qualified as Partial
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import RIO.State
import RIO.Text qualified as Text

import Codegen (pack)
import LambdaMan.Pictures
import LambdaMan.Types
import ProgCon.Eval
import ProgCon.Parser

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Trans.Cofree (CofreeF ((:<)))
import Data.Array hiding (array, index)
import Data.Array.MArray hiding (index)
import Data.Array.ST.Safe hiding (index)
import Data.Functor.Compose
import Data.Functor.Foldable
import Data.List.Extra qualified as List
import Data.Text.IO qualified as Text
import Graphics.Gloss qualified as Gloss
import Graphics.Gloss.Interface.Pure.Simulate qualified as Gloss
import Linear

readBoard :: FilePath -> IO Board
readBoard path = do
  text <- Text.readFile path
  pure do makeBoard text

makeBoardArray :: Text -> Array (V2 Int) Spot
makeBoardArray text = runSTArray do
  board <- newArray (V2 0 0, V2 (width - 1) (height - 1)) Floor
  forM_ (rows `zip` [0 ..]) \(row, j) ->
    forM_ (row `zip` [0 ..]) \(character, i) ->
      writeArray board (V2 i j) (meaning character)
  pure board
 where
  rows = (fmap Text.unpack . Text.lines) text
  height = List.genericLength rows
  width = Partial.maximum (0 :| fmap List.genericLength rows)

makeBoard :: Text -> Board
makeBoard text =
  let
    array = makeBoardArray text
    me = (fst . fromMaybe (error "There should be a Lambda Man somewhere!") . List.find (\(_, spot) -> spot == Me) . assocs) array
  in
    Board {..}

meaning :: Char -> Spot
meaning = \case
  '.' -> Floor
  '#' -> Wall
  'L' -> Me
  character -> error do "Meaningless character " <> show character <> "!"

drawBoard :: Natural -> IO ()
drawBoard level = do
  board <- readBoard ("examples/lambdaman/" <> show level <> "/problem.string")
  let spanningTree = computeSpanningTree board
  let solution = computeOptimalTraversal spanningTree
  -- Gloss.display Gloss.FullScreen backgroundColour  (pictureOfAnimationState AnimationState {..})
  Gloss.simulate Gloss.FullScreen backgroundColour 4 AnimationState {..} pictureOfAnimationState animation

animation :: Gloss.ViewPort -> Float -> AnimationState -> AnimationState
animation _ _ currentAnimationState@AnimationState {..} = case ByteArray.uncons solution of
  Nothing -> currentAnimationState
  Just (directionOfWord8 -> direction, solutionTail) ->
    AnimationState
      { board = (markLegalSteps . fromMaybe (error "Legal step failed!") . flip makeStep direction . unmarkLegalSteps) board
      , solution = solutionTail
      , ..
      }

(?) :: (Ix index) => Array index value -> index -> Maybe value
array ? index
  | inRange (bounds array) index = Just do array ! index
  | otherwise = Nothing
infixl 9 ?

canStepOnThis :: Spot -> Bool
canStepOnThis = \case
  Wall -> False
  _ -> True

canStepOnThisWithoutRetracing :: Spot -> Bool
canStepOnThisWithoutRetracing = \case
  Floor -> True
  LegalStep -> True
  _ -> False

makeStep :: Board -> Direction -> Maybe Board
makeStep Board {..} direction =
  let newMe = me + vectorOfDirection direction
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

readArraySafely :: (Ix index, MArray array value monad) => array index value -> index -> monad (Maybe value)
readArraySafely array index = do
  boundsOfArray <- getBounds array
  if inRange boundsOfArray index
    then fmap Just do readArray array index
    else pure Nothing

legalStepsWithoutRetracing :: STArray lock (V2 Int) Spot -> V2 Int -> ST lock [Direction]
legalStepsWithoutRetracing array me = flip filterM [minBound .. maxBound] \direction -> do
  let index = me + vectorOfDirection direction
  tile <- readArraySafely array index
  pure do maybe False canStepOnThisWithoutRetracing tile

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

type gunctor . functor = Compose gunctor functor
infixr 9 .

unfoldPaths :: V2 Int -> (StateT (STArray lock (V2 Int) Spot) (ST lock) . CofreeF (Map Direction) (V2 Int)) (V2 Int)
unfoldPaths me = Compose do
  mutableArray <- get
  steps <- lift do legalStepsWithoutRetracing mutableArray me
  let ways = flip fmap steps \step -> (step, me + vectorOfDirection step)
  lift do writeArray mutableArray me FootStep
  pure do me :< Map.fromList ways

unfoldWithEffects
  :: (Recursive recursive, Corecursive recursive, Traversable (Base recursive), Monad effect)
  => (carrier -> (effect . Base recursive) carrier)
  -> carrier
  -> effect recursive
unfoldWithEffects = refold sequencer
 where
  sequencer
    :: (Corecursive recursive, Traversable (Base recursive), Monad effect)
    => Compose effect (Base recursive) (effect recursive)
    -> effect recursive
  sequencer = fmap embed . (=<<) sequence . getCompose

unfoldSpanningTree :: V2 Int -> StateT (STArray lock (V2 Int) Spot) (ST lock) (Cofree (Map Direction) (V2 Int))
unfoldSpanningTree = unfoldWithEffects unfoldPaths

computeSpanningTree :: Board -> Cofree (Map Direction) (V2 Int)
computeSpanningTree Board {..} = runST do
  starray <- thaw array
  evalStateT statefulSpanningTree starray
 where
  statefulSpanningTree = unfoldSpanningTree me :: StateT (STArray lock1 (V2 Int) Spot) (ST lock1) (Cofree (Map Direction) (V2 Int))

reversePath :: ByteArray -> ByteArray
reversePath = ByteArray.reverse . ByteArray.map (word8OfDirection . reverseDirection . directionOfWord8)

retractPaths :: [ByteArray] -> [ByteArray]
retractPaths = fix \recurse -> \case
  [] -> []
  (path : paths) -> path : (simplifyPath . reversePath) path : recurse paths

pathToDirectionVectors :: ByteArray -> [V2 Int]
pathToDirectionVectors = fmap (vectorOfDirection . directionOfWord8) . ByteArray.unpack

pathToDirections :: ByteString -> [Direction]
pathToDirections = fmap directionOfWord8 . ByteArray.unpack

directionsToPath :: [Direction] -> ByteArray
directionsToPath = ByteArray.pack . fmap word8OfDirection

computeOptimalTraversal :: Cofree (Map Direction) anything -> ByteArray
computeOptimalTraversal = fold \(_ :< ways) ->
  let
    sorted = (List.sortOn (ByteArray.length . snd) . Map.toList) ways
    attached = fmap (uncurry attachPathInDirection) sorted
  in
    concatenatePaths attached
 where
  attachPathInDirection direction path = word8OfDirection direction `ByteArray.cons` path
  concatenatePaths paths = case List.unsnoc paths of
    Nothing -> ByteArray.empty
    Just (otherPaths, longestPath) -> ByteArray.concat (retractPaths otherPaths) <> longestPath

expressionToBoard :: Expr -> Board
expressionToBoard expression = case evalExpr expression of
  Right (EStr text) -> makeBoard text
  Left errorMessage -> error errorMessage
  Right otherExpression -> error (show otherExpression)

solutionToExpression :: Natural -> ByteArray -> Expr
solutionToExpression nr bs =
  Codegen.pack
    . Text.pack
    . unwords
    $ ["solve", "lambdaman" <> show nr, concatMap show $ pathToDirections bs]

solve :: Board -> ByteArray
solve = computeOptimalTraversal . computeSpanningTree

tracePath :: ByteArray -> [V2 Int]
tracePath = List.scanl (+) 0 . pathToDirectionVectors

simplifyPath :: ByteArray -> ByteArray
simplifyPath path =
  let
    traced = zip (pathToDirections path) (tracePath path)
    simplified = simplifyReversing Set.empty [] traced
  in
    (directionsToPath . reverse . fmap fst) simplified
 where
  simplifyReversing :: Set (V2 Int) -> [(Direction, V2 Int)] -> [(Direction, V2 Int)] -> [(Direction, V2 Int)]
  simplifyReversing = fix \recurse visited simplifiedInReverse -> \case
    [] -> simplifiedInReverse
    ((direction, me) : remaining) ->
      if me `Set.member` visited
        then
          let
            (loop, ((_, _samePlace) : optimal)) = List.break ((== me) . snd) simplifiedInReverse
            notVisitedActually = (Set.fromList . fmap snd) loop
          in
            recurse (visited Set.\\ notVisitedActually) ((direction, me) : optimal) remaining
        else recurse (Set.insert me visited) ((direction, me) : simplifiedInReverse) remaining

validateSolution :: Board -> ByteArray -> Either String Float
validateSolution Board {..} solution = case (filter (\spot -> not (spot == Wall || spot == FootStep)) . elems) filledArray of
  [] -> Right do 100 / fromIntegral (ByteArray.length solution)
  skippedSpots -> Left do show skippedSpots
 where
  filledArray :: Array (V2 Int) Spot
  filledArray = runSTArray do
    mutableArray <- thaw array
    forM_ (tracePath solution) \indexAtZero ->
      let index = indexAtZero + me in writeArray mutableArray index FootStep
    pure mutableArray

solveExpression :: Natural -> Expr -> Either String Expr
solveExpression nr = Right . solutionToExpression nr . solve . expressionToBoard

validateExpression :: Expr -> Either String Float
validateExpression expression = let board = expressionToBoard expression in validateSolution board (solve board)
