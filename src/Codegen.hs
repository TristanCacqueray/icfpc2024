-- |

module Codegen where

import RIO
import Data.List.CommonSubstring
import Data.Text qualified as T

import ProgCon.Parser

pack :: Text -> Expr
pack = toTerm . splitLongest

toTerm :: (Text, [Maybe Text]) -> Expr
toTerm (longest, xs) = EBinary '$' (ELam 1 (mkBody xs)) (EStr longest)
  where
    mkBody [] = error "empty body!"
    mkBody [i] = itemTerm i
    mkBody (i:is) = EBinary '.' (itemTerm i) (mkBody is)

    itemTerm = \case
      Nothing -> EVar 1
      Just txt -> EStr txt

splitLongest :: Text -> (Text, [Maybe Text])
splitLongest txt
  | T.length longest > 3 = (longest, removeLongest [] txt)
  | otherwise = (longest, [])
  where
    (T.unpack -> x, T.unpack -> y) = T.splitAt (T.length txt `div` 2) txt
    longest = T.pack $ longestSubstring x y

    removeLongest acc rest = case T.breakOn longest rest of
      ("", "") -> reverse acc
      (last', "") -> reverse (Just last' : acc)
      (last', matched) -> case T.splitAt (T.length longest) matched of
        (_, remaining) -> case last' of
          "" -> removeLongest (Nothing : acc) remaining
          _ -> removeLongest (Nothing : Just last' : acc) remaining
