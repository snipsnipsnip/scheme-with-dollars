
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Cont
import Control.Applicative
import List
import IO
import Directory
import SexpParser
import Interpreter

load :: FilePath -> IO (V, Env)
load file = do
    bracket (openFile file ReadMode) hClose $ \h -> do
      contents <- hGetContents h
      run contents

repl :: IO ()
repl = do
    b <- doesFileExist "boot.scm"
    env <- if b
        then fmap snd $ load "boot.scm"
        else return [prims]
    loop env $ \env -> do
    do
        putStr "> "
        line <- getLine
        (result, env) <- runEI env line
        print result
        return $ Just env
    `catch` \e -> do
        unless (isEOFError e) (printError e)
        return $ do
            guard $ isUserError e
            return env
    where
    printError e = do
        putStr "error: "
        print e
    loop v m = do
        mv <- m v
        maybe (return ()) (flip loop m)  mv

run :: String -> IO (V, Env)
run = runEI [prims]

runEI :: Env -> String -> IO (V, Env)
runEI env code = case parseManySexp code of
    Left e -> return (U $ "Parse error: " ++ e, env)
    Right (s, _) -> runI env $ evalBegin s

prims :: Frame
prims = flip execState [] list
    where
    subr name f = add name (Subr f)
    syntax name s = add name (Syntax s)
    alias name n = modify $ \dict ->
        let Just prim = lookup n dict in (name, prim) : dict
    add name prim = modify ((name, Prim name prim):)
    
    list = do
        syntax "begin" evalBegin
        syntax "define" evalDefine
        syntax "lambda" evalLambda
        syntax "quote" $ \[e] -> return $ sToV e
        syntax "let" evalLet
        syntax "if" evalIf
        
        subr "print" $ \vs -> do
            liftIO $ mapM_ print vs
            return $ U "print"
        
        alias "p" "print"
        alias "^" "lambda"
        
        subr "+" $ evalNum (+) $ Left 0
        subr "-" $ evalNum (-) $ Right negate
        subr "*" $ evalNum (*) $ Left 1
        subr "/" $ evalNum (/) $ Right recip
        
        subr "<" $ evalCompare (<)
        subr ">" $ evalCompare (>)
        subr "<=" $ evalCompare (<=)
        subr ">=" $ evalCompare (>=)
        subr "==" $ evalCompare (==)
        alias "=" "=="
        
        subr "car" $ \[L (x:_)] -> return x
        subr "cdr" $ \[L (_:xs)] -> return $ L xs
        subr "cons" $ \[x, L xs] -> return $ L (x:xs)
        subr "null" $ \[L xs] -> return $ A $ Bool $ case xs of
            [] -> True
            _ -> False
        
        let io f vs = liftIO $ do
            f vs `catch` \e -> do
                return $ U $ "io error: " ++ show e
        
        subr "pwd" $ io $ \[] -> do
            fmap (A . Str) getCurrentDirectory
        
        subr "chdir" $ io $ \[A (Str path)] -> do
            setCurrentDirectory path
            return $ U "chdir"

        subr "ls" $ io $ \[] -> do
            fmap (L . map (A . Str)) $ getDirectoryContents "."

        subr "mv" $ io $ \[A (Str from), A (Str to)] -> do
            renameFile from to
            return $ U "mv"

evalDefine [S (Left (Sym name)), exp] = do
    val <- eval exp
    env <- getEnv
    setEnv $ defineEnv name val env
    return $ U $ "defined '" ++ name ++ "'"

defineEnv :: String -> V -> Env -> Env
defineEnv name value [] = [makeFrame [(name, value)]]
defineEnv name value (f:fs) = addFrame name value f : fs

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
    env <- getEnv
    apply (F env (Left vars) body) exps
    where
    makePair (S (Right [S (Left (Sym name)), expr])) = return (name, expr)
    makePair _ = fail "syntax error"

evalLet _ = fail "syntax error"
