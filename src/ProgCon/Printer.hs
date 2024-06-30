module ProgCon.Printer where

import Data.Text qualified as T
import RIO

import ProgCon.Parser

printer :: Expr -> Text
printer = ProgCon.Printer.print

print :: Expr -> Text
print = \case
  EBool True -> T.singleton 'T'
  EBool False -> T.singleton 'F'
  EInt nat -> T.cons 'I' (int2str nat)
  EStr text -> T.cons 'S' (encodeString text)
  EUnary c expr -> T.unwords [T.cons 'U' (T.singleton c), printer expr]
  EBinary c expr1 expr2 -> T.unwords [T.cons 'B' (T.singleton c), printer expr1, printer expr2]
  EIf condition expr1 expr2 -> T.unwords ["?", printer condition, printer expr1, printer expr2]
  ELam nat body -> T.unwords [T.cons 'L' (int2str nat), printer body]
  EVar nat -> T.cons 'v' (int2str nat)
