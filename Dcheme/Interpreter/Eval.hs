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
