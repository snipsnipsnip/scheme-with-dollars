module Dcheme.Interpreter.Eval
( eval
, apply
, evalApply
, evalBegin
, evalLambda
) where

import Dcheme.Interpreter.I
import Dcheme.Sexp

eval :: S -> I V
eval (S s) = case s of
    Right [] -> return $ L []
    Right (x:xs) -> do
        x <- eval x
        apply x xs
    Left (Sym a) -> do
        lookupName a
    Left a -> return $ A a

apply :: V -> [S] -> I V
apply (Prim _ (Syntax s)) args = do
    s args
apply v args = do
    values <- mapM eval args
    evalApply v values

evalApply :: V -> [V] -> I V
evalApply (Prim _ (Subr s)) values = do
    s values
evalApply v@(F env argspec f) values = do
    frame <- fmap makeFrame $ bind argspec values
    withEnv (addEnv frame) $ do
      evalBegin f
    where
    bind (Left argnames) values
        | arity == passed = return $ zip argnames values
        | otherwise = fail $ show arity ++ " arguments expected, but got " ++ show passed ++ " for " ++ show v
        where
        arity = length argnames
        passed = length values
    bind (Right argname) values = do
        return [(argname, L values)]
evalApply v _ = fail $ "non applicable value " ++ show v

evalBegin :: Syntax
evalBegin [] = return $ U "empty begin"
evalBegin [s] = eval s
evalBegin (s:ss) = eval s >> evalBegin ss

evalLambda :: Syntax
evalLambda (S argspec:body) = case argspec of
    Right names -> do
        args <- mapM getName names
        env <- getEnv
        return $ F env (Left args) body
    Left (Sym name) -> do
        env <- getEnv
        return $ F env (Right name) body
    _ -> fail "invalid arg spec"
    where
    getName (S (Left (Sym name))) = return name
    getName _ = fail "invalid arg spec"

evalLambda _ = fail $ "malformed lambda"

