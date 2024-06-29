module ProgCon.Eval where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import ProgCon.Parser
import RIO

type Env = Map Natural Expr

evalExpr :: Env -> Expr -> Either String Expr
evalExpr env expr = case expr of
  EUnary UNeg e | Right (EInt x) <- evalExpr env e -> pure $ EInt (negate x)
  EUnary UNeg e -> Left $ "Bad uneg: " <> show e
  EUnary UNot e | Right (EBool x) <- evalExpr env e -> pure $ EBool (not x)
  EUnary UNot e -> Left $ "Bad unot: " <> show e
  EUnary Ustr2int e | Right (EStr x) <- evalExpr env e -> pure $ EInt $ str2int x
  EUnary Uint2str e | Right (EInt x) <- evalExpr env e -> pure $ EStr $ int2str x
  EUnary {} -> Left $ "Bad unary: " <> show expr
  EBinary op e1 e2
    | Right (EInt x) <- evalExpr env e1
    , Right (EInt y) <- evalExpr env e2
    , Just intOp <- integerOp op ->
        pure $ intOp x y
  EBinary op e1 e2
    | Right (EInt x) <- evalExpr env e1
    , Right (EInt y) <- evalExpr env e2
    , Just intComp <- integerComp op ->
        pure $ EBool (intComp x y)
  EBinary op e1 e2
    | Right (EBool x) <- evalExpr env e1
    , Right (EBool y) <- evalExpr env e2
    , Just boolComp <- booleanComp op ->
        pure $ EBool (boolComp x y)
  EBinary '=' e1 e2
    | Right (EStr x) <- evalExpr env e1
    , Right (EStr y) <- evalExpr env e2 ->
        pure $ EBool (x == y)
  EBinary '$' e1 e2 | Right (ELam var body) <- evalExpr env e1 -> evalExpr env $ substitute var e2 body
  EBinary '.' e1 e2
    | Right (EStr s1) <- evalExpr env e1
    , Right (EStr s2) <- evalExpr env e2 ->
        pure $ EStr (s1 <> s2)
  EBinary 'T' e1 e2
    | Right (EInt i) <- evalExpr env e1
    , Right (EStr s) <- evalExpr env e2 ->
        pure $ EStr (T.take (fromIntegral i) s)
  EBinary 'D' e1 e2
    | Right (EInt i) <- evalExpr env e1
    , Right (EStr s) <- evalExpr env e2 ->
        pure $ EStr (T.drop (fromIntegral i) s)
  EBinary {} -> Left $ "Failed to evaluate: " <> show expr <> ", with env: " <> show env
  EVar v -> case Map.lookup v env of
    Nothing -> Left $ "Unbound var: " <> show v <> ", in " <> show env <> ", for " <> show expr
    Just e -> evalExpr env e
  EIf e1 e2 e3
    | Right (EBool pred') <- evalExpr env e1 -> evalExpr env (if pred' then e2 else e3)
  e -> pure e

substitute :: Natural -> Expr -> Expr -> Expr
substitute name expr = \case
  EUnary o e -> EUnary o $ substitute name expr e
  EBinary o e1 e2 -> EBinary o (substitute name expr e1) (substitute name expr e2)
  EVar v | v == name -> expr
         | otherwise -> EVar v
  EIf e1 e2 e3 -> EIf (substitute name expr e1) (substitute name expr e2) (substitute name expr e3)
  ELam v b | v == name -> ELam v b
           | otherwise -> ELam v (substitute name expr b)
  e -> e

integerOp :: Char -> Maybe (Natural -> Natural -> Expr)
integerOp = \case
  '+' -> Just (\x y -> EInt (x + y))
  '-' -> Just (\x y -> if x > y then EInt (x - y) else EUnary UNeg (EInt (y - x)))
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

emptyEnvironment :: Env
emptyEnvironment = Map.empty
