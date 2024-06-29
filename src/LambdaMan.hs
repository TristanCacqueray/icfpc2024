module LambdaMan where

import ProgCon.Parser

solveExpression :: Expr -> Either String Expr
solveExpression = (const . Right) (EStr "LLLDURRRUDRRURR")

validateExpression :: Expr -> Either String Float
validateExpression = const (Left "Not implemented.")
