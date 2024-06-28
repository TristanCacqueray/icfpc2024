module ProgCon.Eval where

import Data.Map.Strict qualified as Map
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
    EUnary{} -> Left $ "Bad unary: " <> show expr
    EBinary '+' e1 e2
        | Right (EInt x) <- evalExpr env e1
        , Right (EInt y) <- evalExpr env e2 ->
            pure $ EInt (x + y)
    EBinary '*' e1 e2
        | Right (EInt x) <- evalExpr env e1
        , Right (EInt y) <- evalExpr env e2 ->
            pure $ EInt (x * y)
    EBinary '$' (ELam var body) e2 -> evalExpr (Map.insert var e2 env) body
    EBinary '$' e1 e2 | Right (ELam var body) <- evalExpr env e1 -> evalExpr (Map.insert var e2 env) body
    EBinary '.' e1 e2
        | Right (EStr s1) <- evalExpr env e1
        , Right (EStr s2) <- evalExpr env e2 ->
            pure $ EStr (s1 <> s2)
    EBinary{} -> Left $ "TODO: " <> show expr
    EVar v -> case Map.lookup v env of
        Nothing -> Left $ "Unbound var: " <> show v <> ", in " <> show env <> ", for " <> show expr
        Just e -> evalExpr env e
    ELam v b -> ELam v <$> evalExpr env b
    e -> pure e

str2int :: Text -> Integer
str2int = undefined

int2str :: Integer -> Text
int2str = undefined
