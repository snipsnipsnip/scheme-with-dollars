{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}

module Interpreter
( I ()
, runI
, V (..)
, Env (..)
, Frame (..)
, Prim (..)
, Subr
, Syntax
, makeFrame
, addFrame
, addEnv
, getEnv
, setEnv
, withEnv
, eval
, apply
, evalApply
, evalBegin
, evalLambda
) where

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Cont
import Control.Applicative
import Data.List
import System.IO
import SexpParser

newtype I a = I
    { unI :: ErrorT String (StateT Env (ContT (V, Env) IO)) a
    } deriving
    ( Monad
    , Functor
    , MonadIO
    , MonadCont
    , MonadError String
    )

runI :: Env -> I V -> IO (V, Env)
runI env (I i) = flip runContT return $ flip runStateT env $ fmap coerceError $ runErrorT i
    where
    coerceError (Left msg) = U $ "error: " ++ msg
    coerceError (Right v) = v

getEnv :: I Env
getEnv = I get

setEnv :: Env -> I ()
setEnv = I . put

withEnv :: (Env -> Env) -> I a -> I a
withEnv f = I . ErrorT . sandbox . withStateT f . runErrorT . unI
  where
  sandbox :: Monad m => StateT s m a -> StateT s m a
  sandbox m = StateT $ \s -> do
      (a, _) <- runStateT m s
      return (a, s)

type Frame = [(String, V)]
addFrame name value frame = (name, value) : frame
makeFrame kvs = kvs :: Frame
findFrame name frame = lookup name frame

type Env = [Frame]
addEnv frame env = frame : env
findEnv name env = msum $ map (findFrame name) env

showEnv :: Env -> String
showEnv e = showRoundList (map (showRoundList . map (showString . fst)) e) ""

lookupName :: String -> I V
lookupName name = I $ do
    r <- gets $ findEnv name
    maybe (fail $ "variable |" ++ name ++ "| not found") checkBottom r
    where
    checkBottom (Bottom msg) = fail msg
    checkBottom v = return v

data V
    = A Atom
    | L [V]
    | U String -- undefined harmless value
    | F Env (Either [String] String) [S]
    | Prim String Prim
    | Bottom String -- fails when applied

data Prim
    = Subr Subr
    | Syntax Syntax

type Subr = [V] -> I V
type Syntax = [S] -> I V

instance Show V where
    showsPrec _ v = case v of
        A a -> shows a
        L [A (Sym "quote"), s] -> showChar '\'' . shows s
        L l -> showRoundList (map shows l)
        U s -> val "@" $ showString s
        F env argspec body -> val "func" $ case argspec of
            Left args -> showRoundList (map showString args)
            Right arg -> showString arg
        Prim name prim -> val (primName prim) $ showString name
        Bottom b -> val "bottom" $ showString b
        where
        val name s = showString "#<" . showString name . showChar ' ' . s . showChar '>'
        primName (Subr _) = "subr"
        primName (Syntax _) = "syntax"

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

