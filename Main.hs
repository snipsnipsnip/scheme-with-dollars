
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Cont
import Control.Applicative
import Data.List
import System.IO
import System.Directory
import CexpParser
import Interpreter
import Control.Exception (try, evaluate, PatternMatchFail (..), bracket)
import System.IO.Error (isEOFError, isUserError, catchIOError)

load :: FilePath -> IO (V, Env)
load file = do
    bracket (openFile file ReadMode) hClose $ \h -> do
      contents <- hGetContents h
      run contents

repl :: IO ()
repl = do
    b <- doesFileExist "init.scm"
    env <- if b
        then fmap snd $ load "init.scm"
        else return [prims]
    loop env $ \env -> do
    do
        putStr "> "
        line <- getLine
        (result, env) <- runEI env line
        print result
        return $ Just env
    `catchIOError` \e -> do
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
runEI env code = case parseManyCexp code of
    Left e -> return (U $ "Parse error: " ++ e, env)
    Right (s, _) -> runI env $ evalBegin s

prims :: Frame
prims = flip execState [] list
    where
    alias name n = modify addAlias
        where
        addAlias dict = let Just prim = lookup n dict in (name, prim) : dict
    add typ name f = modify ((name, Prim name $ typ $ wrap f):)
        where
        wrap f vs = do
            e <- liftIO $ try $ evaluate $ f vs
            either err id e
        err (PatternMatchFail _) = do
            fail $ "argument error: " ++ name
    subr = add Subr
    syntax = add Syntax
    
    list = do
        syntax "begin" evalBegin
        syntax "define" evalDefine
        syntax "lambda" evalLambda
        syntax "quote" $ \[e] -> return $ sToV e
        syntax "let" evalLet
        syntax "letrec" evalLetrec
        syntax "if" evalIf
        
        subr "print" $ \vs -> do
            liftIO $ mapM_ print vs
            return $ U "print"
        
        alias "p" "print"
        alias "^" "lambda"
        
        syntax "and" $ evalLogic False
        syntax "or" $ evalLogic True
        subr "not" $ \[x] -> return $ A $ Bool $ not $ isTrue $ x
        
        subr "+" $ evalNum (+) $ Left 0
        subr "-" $ evalNum (-) $ Right negate
        subr "*" $ evalNum (*) $ Left 1
        subr "/" $ evalNum (/) $ Right recip
        subr "mod" $ \[va, vb] -> do
            a <- unnum va
            b <- unnum vb
            return $ A $ Num $ fromIntegral $ mod (floor a) (floor b)
        
        subr "<" $ evalCompare (<)
        subr ">" $ evalCompare (>)
        subr "<=" $ evalCompare (<=)
        subr ">=" $ evalCompare (>=)
        subr "==" $ evalCompare (==)
        alias "=" "=="
        
        subr "car" $ \[L (x:_)] -> return x
        subr "cdr" $ \[L (_:xs)] -> return $ L xs
        subr "cons" $ \[x, L xs] -> return $ L (x:xs)
        subr "null?" $ \[L xs] -> return $ A $ Bool $ null xs
        subr "apply" $ \[f, L xs] -> evalApply f xs
        
        let io f vs = liftIO $ do
            f vs `catchIOError` \e -> do
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

evalDefine (S (Right (S (Left (Sym name)):argspec)):body) = do
    val <- evalLambda (S (Right argspec):body)
    env <- getEnv
    setEnv $ defineEnv name val env
    return $ U $ "defined proc '" ++ name ++ "'"
evalDefine _ = fail "malformed define"

defineEnv :: String -> V -> Env -> Env
defineEnv name value [] = [makeFrame [(name, value)]]
defineEnv name value (f:fs) = addFrame name value f : fs

sToV :: S -> V
sToV (S s) = case s of
    Left a -> A a
    Right ss -> L (map sToV ss)

evalNum :: (Double -> Double -> Double) -> Either Double (Double -> Double) -> Subr
evalNum _ (Left d) [] = return $ A $ Num d
evalNum _ (Right _) [] = fail $ "procedure requires at least one argument"
evalNum _ (Left _) [n] = return n
evalNum _ (Right op) [nv] = fmap (A . Num . op) $ unnum nv
evalNum op _ xs = fmap (A . Num . foldl1 op) $ mapM unnum xs

unnum (A (Num x)) = return x
unnum x = fail $ "number expected, but got " ++ show x

evalLogic :: Bool -> Syntax
evalLogic b = loop
  where
  loop [] = return $ A $ Bool b
  loop [x] = eval x
  loop (x:xs) = do
    v <- eval x
    if isTrue v == b
      then return v
      else loop xs

evalCompare :: (Atom -> Atom -> Bool) -> Subr
evalCompare op [A a, A b] = do
    return $ A $ Bool $ a `op` b
evalCompare _ _ = fail "compare: 2 args expected"

evalIf :: Syntax
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

evalLet :: Syntax
evalLet (S (Right bindings):body) = do
    (vars, exps) <- unzipLetlist bindings
    env <- getEnv
    apply (F env (Left vars) body) exps

unzipLetlist :: [S] -> I ([String], [S])
unzipLetlist bindings = fmap unzip $ mapM makePair bindings
    where
    makePair (S (Right [S (Left (Sym name)), expr])) = return (name, expr)
    makePair _ = fail "syntax error"

evalLetrec :: Syntax
evalLetrec (S (Right bindings):body) = do
    (vars, exps) <- unzipLetlist bindings
    let newFrame = makeFrame [(v, Bottom $ "letrec dummy for " ++ v) | v <- vars]
    vals <- withEnv (addEnv newFrame) $ mapM eval exps
    env <- getEnv
    evalApply (F env (Left vars) body) vals

evalLetrec _ = fail "syntax error"