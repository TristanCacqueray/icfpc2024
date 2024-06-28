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
    deriving (Show)

parseExpr :: Text -> Either String Expr
parseExpr = P.parseOnly exprP

exprP :: P.Parser Expr
exprP = taggedP 'S' EStr stringP
  where
    taggedP c tag p = P.char c *> (tag <$> p)

stringP :: P.Parser Text
stringP = T.map convertChars <$> takeString
  where
    takeString = P.takeWhile (\c -> c >= '!' && c <= '~')
    convertChars c = charOrder !! (fromEnum c - 33)

charOrder :: String
charOrder = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

encodeString :: Text -> Text
encodeString message = T.cons 'S' $ T.map convertChars message
  where
    convertChars c = toEnum $ 33 + fromMaybe 0 (elemIndex c charOrder)
