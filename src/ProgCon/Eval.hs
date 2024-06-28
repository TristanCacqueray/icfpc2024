module ProgCon.Eval where

import RIO
import ProgCon.Parser
import Data.Map.Strict qualified as Map

type Env = Map Natural Expr

evalExpr :: Env -> Expr -> Either String Expr
evalExpr env expr = case expr of
  EUnary UNeg (EInt x) -> pure $ EInt (negate x)
  EUnary UNeg e -> Left $ "Bad uneg: " <> show e
  EUnary UNot (EBool x) -> pure $ EBool (not x)
  EUnary UNot e -> Left $ "Bad unot: " <> show e
  EUnary Ustr2int (EStr x) -> pure $ EInt $ str2int x
  EUnary Uint2str (EInt x) -> pure $ EStr $ int2str x
  EUnary _ _ -> Left $ "Bad unary: " <> show expr
  EBinary '+' (EInt x) (EInt y) -> pure $ EInt (x + y)
  EBinary '$' e1 e2 -> case evalExpr env e1 of
    Right (ELam var body) -> evalExpr (Map.insert var e2 env) body
    _ -> Left $ "Bad apply op: " <> show expr
  EBinary _ _ _ -> Left $ "TODO: " <> show expr
  EVar v -> case Map.lookup v env of
    Nothing -> Left $ "Unbound var: " <> show v <> ", in " <> show env <> ", for " <> show expr
    Just e -> pure e
  e -> pure e


str2int :: Text -> Integer
str2int = undefined

int2str :: Integer -> Text
int2str = undefined
