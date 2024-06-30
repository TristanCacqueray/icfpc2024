module ProgCon.Eval where

import Data.Text qualified as T
import ProgCon.Parser
import RIO

evalExpr :: Expr -> Either String Expr
evalExpr expr = case expr of
  EUnary '-' e | Right (EInt x) <- evalExpr e -> pure $ EInt (negate x)
  EUnary '-' e -> Left $ "Bad uneg: " <> show e
  EUnary '!' e | Right (EBool x) <- evalExpr e -> pure $ EBool (not x)
  EUnary '!' e -> Left $ "Bad unot: " <> show e
  EUnary '#' e | Right (EStr x) <- evalExpr e -> pure $ EInt $ str2int x
  EUnary '$' e | Right (EInt x) <- evalExpr e -> pure $ EStr $ int2str x
  EUnary {} -> Left $ "Bad unary: " <> show expr
  EBinary op e1 e2
    | Right (EInt x) <- evalExpr e1
    , Right (EInt y) <- evalExpr e2
    , Just intOp <- integerOp op ->
        pure $ intOp x y
  EBinary op e1 e2
    | Right (EInt x) <- evalExpr e1
    , Right (EInt y) <- evalExpr e2
    , Just intComp <- integerComp op ->
        pure $ EBool (intComp x y)
  EBinary op e1 e2
    | Right (EBool x) <- evalExpr e1
    , Right (EBool y) <- evalExpr e2
    , Just boolComp <- booleanComp op ->
        pure $ EBool (boolComp x y)
  EBinary '=' e1 e2
    | Right (EStr x) <- evalExpr e1
    , Right (EStr y) <- evalExpr e2 ->
        pure $ EBool (x == y)
  EBinary '$' e1 e2 | Right (ELam var body) <- evalExpr e1 -> evalExpr $ substitute var e2 body
  EBinary '.' e1 e2
    | Right (EStr s1) <- evalExpr e1
    , Right (EStr s2) <- evalExpr e2 ->
        pure $ EStr (s1 <> s2)
  EBinary 'T' e1 e2
    | Right (EInt i) <- evalExpr e1
    , Right (EStr s) <- evalExpr e2 ->
        pure $ EStr (T.take (fromIntegral i) s)
  EBinary 'D' e1 e2
    | Right (EInt i) <- evalExpr e1
    , Right (EStr s) <- evalExpr e2 ->
        pure $ EStr (T.drop (fromIntegral i) s)
  EBinary {} -> Left $ "Failed to evaluate: " <> show expr
  EVar v -> Left $ "Unbound var: " <> show v
  EIf e1 e2 e3
    | Right (EBool pred') <- evalExpr e1 -> evalExpr (if pred' then e2 else e3)
  e -> pure e

substitute :: Natural -> Expr -> Expr -> Expr
substitute name expr = \case
  EUnary o e -> EUnary o $ substitute name expr e
  EBinary o e1 e2 -> EBinary o (substitute name expr e1) (substitute name expr e2)
  EVar v
    | v == name -> expr
    | otherwise -> EVar v
  EIf e1 e2 e3 -> EIf (substitute name expr e1) (substitute name expr e2) (substitute name expr e3)
  ELam v b
    | v == name -> ELam v b
    | otherwise -> ELam v (substitute name expr b)
  e -> e

integerOp :: Char -> Maybe (Natural -> Natural -> Expr)
integerOp = \case
  '+' -> Just (\x y -> EInt (x + y))
  '-' -> Just (\x y -> if x > y then EInt (x - y) else EUnary '-' (EInt (y - x)))
  '*' -> Just (\x y -> EInt (x * y))
  '/' -> Just (\x y -> EInt (x `div` y))
  '%' -> Just (\x y -> EInt (x `mod` y))
  _ -> Nothing

integerComp :: Char -> Maybe (Natural -> Natural -> Bool)
integerComp = \case
  '<' -> Just (<)
  '>' -> Just (>)
  '=' -> Just (==)
  _ -> Nothing

booleanComp :: Char -> Maybe (Bool -> Bool -> Bool)
booleanComp = \case
  '=' -> Just (==)
  '|' -> Just (||)
  '&' -> Just (&&)
  _ -> Nothing
