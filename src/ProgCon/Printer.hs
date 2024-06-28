module ProgCon.Printer where

import RIO

import ProgCon.Parser

print :: Expr -> Text
print = \case
  EBool bool -> error "Not implemented."
  EInt natural -> error "Not implemented."
  EStr text -> "S" <> encodeString text
  EUnary unaryOp expr -> error "Not implemented."
  EBinary char expr1 expr2 -> error "Not implemented."
  EIf conditional whenTrue whenFalse -> error "Not implemented."
  ELam natural body -> error "Not implemented."
  EVar natural -> error "Not implemented."
