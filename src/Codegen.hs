module Codegen where

import Data.Foldable1
import Data.List.NonEmpty qualified as NE
import Data.Semigroup
import Data.SuffixTree
import Data.Text qualified as T
import RIO

import ProgCon.Parser
import ProgCon.Printer

pack :: Text -> Expr
pack = toTerm . splitLongest

toTerm :: SplitResult -> Expr
toTerm res = case res.repeatSplit of
  Nothing -> rawStr
  Just (longest, xs) ->
    let packedExpr = EBinary '$' (ELam 1 (mkBody xs)) (EStr longest)
    in  if T.length (printer packedExpr) < T.length res.original
          then packedExpr
          else rawStr
 where
  rawStr = EStr res.original
  mkBody [] = error "empty body!"
  mkBody [i] = itemTerm i
  mkBody (i : is) = EBinary '.' (itemTerm i) (mkBody is)

  itemTerm = \case
    Nothing -> EVar 1
    Just txt -> EStr txt

data SplitResult = SplitResult
  { original :: Text
  , repeatSplit :: Maybe (Text, [Maybe Text])
  }

splitLongest :: Text -> SplitResult
splitLongest txt = SplitResult txt repeatSplit
 where
  repeatSplit
    | T.length longest > 3 = Just (longest, removeLongest [] txt)
    | otherwise = Nothing
  longest = T.pack $ longestRepeatedNonOverlapping (T.unpack txt)

  removeLongest acc rest = case T.breakOn longest rest of
    ("", "") -> reverse acc
    (last', "") -> reverse (Just last' : acc)
    (last', matched) -> case T.splitAt (T.length longest) matched of
      (_, remaining) -> case last' of
        "" -> removeLongest (Nothing : acc) remaining
        _ -> removeLongest (Nothing : Just last' : acc) remaining

-- | By @meooow - https://discourse.haskell.org/t/challenge-finding-the-longest-repeated-substring/9890/9
longestRepeatedNonOverlapping :: String -> String
longestRepeatedNonOverlapping s =
  case go 0 [] (construct (s ++ "\0")) of (,,) (Max (Arg _ s')) _ _ -> s'
 where
  go !len _ Leaf = (,,) (Max (Arg 0 "")) (Just (Min len)) (Just (Max len))
  go len pieces (Node es) = case foldMap1' f (NE.fromList es) of
    (,,) best min_@(Just (Min mnx)) max_@(Just (Max mxx)) ->
      let len' = min len (mxx - mnx) -- no overlapping
          substr = take len' (concat (reverse pieces))
      in  (,,) (best <> Max (Arg len' substr)) min_ max_
    _ -> error "impossible"
   where
    f (p, n) = go (len + length (prefix p)) (prefix p : pieces) n
