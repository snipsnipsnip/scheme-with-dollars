{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}

module Dcheme.Interpreter.I
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
, lookupName
) where

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Cont
import Control.Applicative
import Data.List
import System.IO
import Dcheme.Sexp

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
