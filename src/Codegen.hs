module Codegen where

import Data.List.CommonSubstring
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
     in if T.length (printer packedExpr) < T.length res.original
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

data SplitResult = SplitResult {
  original :: Text,
  repeatSplit :: Maybe (Text, [Maybe Text])
                               }

splitLongest :: Text -> SplitResult
splitLongest txt = SplitResult txt repeatSplit
 where
  repeatSplit
    | T.length longest > 3 = Just (longest, removeLongest [] txt)
    | otherwise = Nothing
  (T.unpack -> x, T.unpack -> y) = T.splitAt (T.length txt `div` 2) txt
  longest = T.pack $ longestSubstring x y

  removeLongest acc rest = case T.breakOn longest rest of
    ("", "") -> reverse acc
    (last', "") -> reverse (Just last' : acc)
    (last', matched) -> case T.splitAt (T.length longest) matched of
      (_, remaining) -> case last' of
        "" -> removeLongest (Nothing : acc) remaining
        _ -> removeLongest (Nothing : Just last' : acc) remaining
