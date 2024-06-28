module ProgCon.Printer where

import RIO

import ProgCon.Parser

print :: Expr -> Text
print = \case
  EBool _bool -> error "Not implemented."
  EInt _natural -> error "Not implemented."
  EStr text -> "S" <> encodeString text
  EUnary _unaryOp _expr -> error "Not implemented."
  EBinary _char _expr1 _expr2 -> error "Not implemented."
  EIf _conditional _whenTrue _whenFalse -> error "Not implemented."
  ELam _natural _body -> error "Not implemented."
  EVar _natural -> error "Not implemented."
