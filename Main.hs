{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Applicative
import List
import IO

newtype P a = P (StateT String (Either String) a)
    deriving (Monad, Functor)

instance Applicative P where
    pure = return
    (<*>) = ap

instance Alternative P where
    empty = fail "empty"
    P a <|> P b = P $ a `catchError` const b

instance MonadPlus P where
    mzero = empty
    mplus = (<|>)

runP :: P a -> String -> Either String (a, String)
runP (P p) s = runStateT p s

evalP :: P a -> String -> Either String a
evalP p s = fmap fst $ runP p s

anyChar :: P Char
anyChar = P $ do
    s <- get
    case s of
        d:s -> do
            put s
            return d
        _ -> do
            fail "eof"

char :: Char -> P Char
char c = do
    d <- anyChar
    unless (c == d) $ fail $ ($ "") $
        showString "expected char " .
        showChar c .
        showString ", but got " .
        showChar d
    return d

charExcept :: String -> P Char
charExcept set = do
    c <- anyChar
    unless (c `notElem` set) $ fail $ ($ "") $
        showString "expected char except " .
        shows set .
        showString ", but got " .
        showChar c
    return c

($>) :: Alternative f => f a -> b -> f b
($>) = flip (<$)

peek :: P a -> P (Maybe a)
peek (P p) = P $ do
    s <- get
    return $ eitherToMaybe $ evalStateT p s

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

neg :: Alternative f => f a -> f ()
neg a = a *> empty <|> pure ()

paren :: Char -> Char -> P a -> P a
paren open close p = char open *> p <* char close

--------------

expr :: P Double
expr = summ
    where
    summ = fmap sum $ liftA2 (:) prod $ many $ do
        whitespace
        op <- char '+' $> id <|> char '-' $> negate
        op <$> prod
    prod = fmap product $ liftA2 (:) value $ many $ do
        whitespace
        op <- char '*' $> id <|> char '/' $> recip
        op <$> value
    value = do
        whitespace
        number <|> paren '(' ')' (expr <* whitespace)

whitechar = msum $ map char " \t\n"
whitespace = many whitechar
whitespace1 = some whitechar

number :: P Double
number = do
    fmap read $ some $ msum $ map char ['0'..'9']

test :: Bool
test = runP expr "1 + 1 * (6/9 )* 3 + 4" == Right (7, "")

------------

newtype S = S (Either Atom [S])

data Atom
    = Str String
    | Sym String
    | Num Double
    | Chr Char
    | Bool Bool
    deriving (Eq, Ord)

showRoundList :: [ShowS] -> ShowS
showRoundList list =
    showParen True $ foldl (.) id $ intersperse (showChar ' ') list

instance Show S where
    showsPrec _ (S s) = either shows shows s
    showList [S (Left (Sym "quote")), s] = showChar '\'' . shows s
    showList list = showRoundList $ map shows list

instance Show Atom where
    showsPrec _ a = case a of
        Str s -> shows s
        Sym s -> showString s
        Num n -> shows n
        Chr c -> showString "#\\" . showString (tail $ init $ show c)
        Bool True -> showString "#t"
        Bool False -> showString "#f"

stringEscapable :: Char -> Char -> P String
stringEscapable marker escape = paren marker marker $ many chr
    where
    chr = quotedChr <|> normalChr
    normalChr = charExcept [marker]
    quotedChr = fmap special $ char escape *> anyChar
    special 'n' = '\n'
    special 't' = '\t'
    special c = c

sexp :: P S
sexp = ws *> sexp1
    where
    sexp1 = fmap S (atom <|> quote <|> list)
    
    quote = fmap wrap $ do
        char '\''
        sexp
        where
        wrap s = Right [S $ Left $ Sym "quote", s]
    
    atom = fmap Left $ str <|> sharp <|> num <|> sym 
        where
        num = fmap Num $ number
        str = fmap Str $ stringEscapable '"' '\\'
        sym = fmap Sym $ name <|> stringEscapable '|' '\\'
        sharp = char '#' >> true <|> false <|> chr
        true = char 't' $> Bool True
        false = char 'f' $> Bool False
        chr = char '\\' *> fmap Chr (special <|> anyChar)
        special = mapM_ char "newline" $> '\n' <|> mapM_ char "space" $> ' '
        
        name = liftA2 (:) first (many rest)
        first = charExcept $ prohibited ++ ".#"
        rest = charExcept prohibited
        prohibited = " \t\n\\,'\"()[]|"
    
    list = fmap Right $ paren '(' ')' $ do
        ws
        list1 <* ws <|> pure []
        where
        list1 = do
            liftA2 (:) sexp1 $ many $ ws1 *> sexp1
    
    ws = whitespace >> comment >> whitespace
    ws1 = whitespace1 >> comment >> whitespace
    comment = mapM_ char "#|" *> eatUntil "|#" <|> return ()

eatUntil :: String -> P ()
eatUntil str = loop str
    where
    loop "" = return ()
    loop (c:cs) = do
        d <- anyChar
        if c == d then loop cs else loop str

parseSexp :: String -> Either String (S, String)
parseSexp = runP sexp

parseManySexp :: String -> Either String ([S], String)
parseManySexp = runP (many sexp)

instance Read S where
    readsPrec _ str = case parseSexp str of
        Right v -> [v]
        Left e -> error e

--

newtype I a = I (StateT Env (ContT V (ErrorT String IO)) a)
    deriving (Monad, Functor, MonadIO, MonadCont)

runI :: Env -> I V -> IO (Either String V)
runI env (I i) = runErrorT (runContT (evalStateT i env) return)

evalI :: Env -> I V -> IO (Either String V)
evalI env i = runI env i

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

mapI f (I i) = I $ f i

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

load :: FilePath -> IO (Either String V)
load file = do
    bracket (openFile file ReadMode) hClose $ \h -> do
      contents <- hGetContents h
      run contents

run :: String -> IO (Either String V)
run code = case parseManySexp code of
    Left e -> return $ Left $ "Parse error:" ++ e
    Right (s, _) -> evalI [prims] $ evalBegin s
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

isTrue (A (Bool False)) = False
isTrue _ = True
