{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Cont
import Control.Applicative
import List
import IO
import SexpParser

newtype I a = I (ErrorT String (StateT Env (ContT V IO)) a)
    deriving (Monad, Functor, MonadIO, MonadCont)

runI :: Env -> I V -> IO V
runI env (I i) = runContT (evalStateT (fmap coerceError $ runErrorT i) env) return
    where
    coerceError (Left msg) = U $ "error: " ++ msg
    coerceError (Right v) = v

type Frame = [(String, V)]
addFrame name value frame = (name, value) : frame
makeFrame kvs = kvs :: Frame
findFrame name frame = lookup name frame

type Env = [Frame]
addEnv frame env = frame : env
findEnv name env = msum $ map (findFrame name) env

defineEnv :: String -> V -> Env -> Env
defineEnv name value [] = [makeFrame [(name, value)]]
defineEnv name value (f:fs) = addFrame name value f : fs

showEnv :: Env -> String
showEnv e = showRoundList (map (showRoundList . map (showString . fst)) e) ""

lookupName :: String -> I V
lookupName name = I $ do
    r <- gets $ findEnv name
    maybe (fail $ "variable |" ++ name ++ "| not found") return r

data V
    = A Atom
    | L [V]
    | U String
    | F Env (Either [String] String) [S]
    | Prim String Prim

data Prim
    = Subr ([V] -> I V)
    | Syntax ([S] -> I V)

instance Show V where
    showsPrec _ v = case v of
        A a -> shows a
        L l -> showRoundList (map shows l)
        U s -> val "undef" $ showString s
        F env argspec body -> val "func" $ case argspec of
            Left args -> showRoundList (map showString args)
            Right arg -> showString arg
        Prim name prim -> val (primName prim) $ showString name
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
    s@(Left a) -> return $ A a

mapI f (I i) = I $ ErrorT $ f $ runErrorT i

sandbox :: Monad m => StateT s m a -> StateT s m a
sandbox m = StateT $ \s -> do
    (a, _) <- runStateT m s
    return (a, s)

apply :: V -> [S] -> I V
apply (U m) _ = fail $ "can't apply undefined: " ++ show m
apply v@(F env argspec f) args = do
    values <- mapM eval args
    mapI sandbox $ do
        I $ put $ flip addEnv env $ makeFrame $ bind argspec values
        evalBegin f
    where
    bind (Left argnames) values = zip argnames values
    bind (Right argname) values = [(argname, L values)]
apply (Prim _ p) args = applyPrim p args
apply (A a) _ = fail $ "can't apply " ++ show a
apply (L l) _ = fail $ "can't apply " ++ show l

applyPrim :: Prim -> [S] -> I V
applyPrim (Subr s) args = do
    values <- mapM eval args
    s values
applyPrim (Syntax s) args = do
    s args

evalBegin :: [S] -> I V
evalBegin [] = return $ U "empty begin"
evalBegin [s] = eval s
evalBegin (s:ss) = eval s >> evalBegin ss

evalDefine [S (Left (Sym name)), exp] = do
    val <- eval exp
    I $ modify $ defineEnv name val
    return $ U $ "defined '" ++ name ++ "'"

evalLambda :: [S] -> I V
evalLambda (S argspec:body) = case argspec of
    Right names -> do
        args <- mapM getName names
        env <- I get
        return $ F env (Left args) body
    Left (Sym name) -> do
        env <- I get
        return $ F env (Right name) body
    _ -> fail "invalid arg spec"
    where
    getName (S (Left (Sym name))) = return name
    getName _ = fail "invalid arg spec"

----

load :: FilePath -> IO V
load file = do
    bracket (openFile file ReadMode) hClose $ \h -> do
      contents <- hGetContents h
      run contents

runRepl = loop $ do
    do
        putStr "> "
        line <- getLine
        result <- run line
        print result
        return True
    `catch` \e -> do
        unless (isEOFError e) (printError e)
        return $ isUserError e
    where
    printError e = do
        putStr "error: "
        print e
    loop m = do
        b <- m
        when b (loop m)

run :: String -> IO V
run code = case parseManySexp code of
    Left e -> return $ U $ "Parse error:" ++ e
    Right (s, _) -> runI [prims] $ evalBegin s
    where
    prims = flip execState [] list
    subr name f = add name (Subr f)
    syntax name s = add name (Syntax s)
    alias name n = modify $ \dict ->
        let Just prim = lookup n dict in (name, prim) : dict
    add name prim = modify ((name, Prim name prim):)
    list = do
        subr "print" $ \vs -> do
            liftIO $ mapM_ print vs
            return $ U "print"
        
        subr "+" $ evalNum (+) $ Left 0
        subr "-" $ evalNum (-) $ Right negate
        subr "*" $ evalNum (*) $ Left 1
        subr "/" $ evalNum (/) $ Right recip
        
        subr "<" $ evalCompare (<)
        subr ">" $ evalCompare (>)
        subr "<=" $ evalCompare (<=)
        subr ">=" $ evalCompare (>=)
        subr "==" $ evalCompare (==)
        subr "=" $ evalCompare (==)
        
        syntax "begin" evalBegin
        syntax "define" evalDefine
        syntax "lambda" evalLambda
        syntax "quote" $ \[e] -> return $ sToV e
        syntax "let" evalLet
        
        alias "p" "print"
        alias "^" "lambda"
        
        syntax "if" evalIf

sToV :: S -> V
sToV (S s) = case s of
    Left a -> A a
    Right ss -> L (map sToV ss)

evalNum :: (Double -> Double -> Double) -> Either Double (Double -> Double) -> [V] -> I V
evalNum _ (Left d) [] = return $ A $ Num d
evalNum _ (Right _) [] = fail "procedure requires at least one argument"
evalNum _ (Left _) [n] = return n
evalNum _ (Right op) [nv] = fmap (A . Num . op) $ unnum nv
evalNum op _ xs = fmap (A . Num . foldl1 op) $ mapM unnum xs

unnum (A (Num x)) = return x
unnum x = fail $ "number expected, but got " ++ show x

evalCompare :: (Atom -> Atom -> Bool) -> [V] -> I V
evalCompare op [A a, A b] = do
    return $ A $ Bool $ a `op` b
evalCompare _ _ = fail "2 args expected"

evalIf [cond, true] = do
    result <- eval cond
    if isTrue result
        then eval true
        else return $ U "else"

evalIf [cond, true, false] = do
    result <- eval cond
    if isTrue result
        then eval true
        else eval false
evalIf _ = fail "syntax error"

isTrue (A (Bool False)) = False
isTrue _ = True

evalLet (S (Right bindings):body) = do
    (vars, exps) <- fmap unzip $ mapM makePair bindings
    env <- I get
    apply (F env (Left vars) body) exps
    where
    makePair (S (Right [S (Left (Sym name)), expr])) = return (name, expr)
    makePair _ = fail "syntax error"

evalLet _ = fail "syntax error"
