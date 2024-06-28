{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ProgCon.Parser where

import Data.Attoparsec.Text qualified as P
import Data.List (elemIndex)
import Data.Text qualified as T
import RIO

data Expr
  = EBool Bool
  | EInt Natural
  | EStr Text
  | EUnary UnaryOp Expr
  | EBinary Char Expr Expr
  | EIf Expr Expr Expr
  | ELam Natural Expr
  | EVar Natural
  deriving (Show, Eq, Ord)

data UnaryOp = UNeg | UNot | Ustr2int | Uint2str
  deriving (Show, Eq, Ord)

parseExpr :: Text -> Either String Expr
parseExpr = P.parseOnly (exprP <* P.endOfInput)

exprP :: P.Parser Expr
exprP = P.skipSpace >> p
 where
  p =
    taggedP 'T' EBool (pure True)
      <|> taggedP 'F' EBool (pure False)
      <|> taggedP 'I' EInt intP
      <|> taggedP 'S' EStr stringP
      <|> (P.char 'U' *> unaryP)
      <|> (P.char 'B' *> binaryP)
      <|> (P.char '?' *> ifP)
      <|> (P.char 'L' *> lambdaP)
      <|> taggedP 'v' EVar natP

lambdaP :: P.Parser Expr
lambdaP = ELam <$> natP <*> exprP

ifP :: P.Parser Expr
ifP = EIf <$> exprP <*> exprP <*> exprP

taggedP :: Char -> (a -> b) -> P.Parser a -> P.Parser b
taggedP c tag p = P.char c *> (tag <$> p)

unaryP :: P.Parser Expr
unaryP =
  taggedP '-' (EUnary UNeg) exprP
    <|> taggedP '!' (EUnary UNot) exprP
    <|> taggedP '#' (EUnary Ustr2int) exprP
    <|> taggedP '$' (EUnary Uint2str) exprP

binaryP :: P.Parser Expr
binaryP = EBinary <$> P.anyChar <*> exprP <*> exprP

asciiP :: P.Parser Text
asciiP = P.takeWhile (\c -> c >= '!' && c <= '~')

stringP :: P.Parser Text
stringP = fmap decodeString asciiP

decodeString :: Text -> Text
decodeString = T.map convertChars
 where
  convertChars c = charOrder !! (fromEnum c - 33)

charOrder :: String
charOrder = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

encodeString :: Text -> Text
encodeString message = T.map convertChars message
 where
  convertChars c = toEnum $ 33 + fromMaybe 0 (elemIndex c charOrder)

intP :: P.Parser Natural
intP = str2int <$> asciiP

str2int :: Text -> Natural
str2int = T.foldl' go 0
 where
  go acc c = acc * 94 + (fromIntegral (fromEnum c) - 33)

digits :: Natural -> String
digits x = if x `div` 94 == 0 then [digit x] else digits (x `div` 94) ++ [digit (x `mod` 94)]

digit :: Natural -> Char
digit x = toEnum (33 + fromIntegral x)

int2str :: Natural -> Text
int2str x = T.pack $ digits x

natP :: P.Parser Natural
natP = T.foldl' go 0 <$> asciiP
 where
  go :: Natural -> Char -> Natural
  go acc c = acc * 94 + (fromIntegral (fromEnum c) - 33)
