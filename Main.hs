{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import Control.Applicative
import List

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

data GS a
    = L [S]
    | A Atom
    | C a

data Atom
    = Str String
    | Sym String
    | Num Double
    deriving Eq

showRoundList :: [ShowS] -> ShowS
showRoundList list =
    showParen True $ foldl (.) id $ intersperse (showChar ' ') list

instance Show a => Show (GS a) where
    showsPrec _ s = case s of
        L [A (Sym "quote"), s] -> showChar '\'' . shows s
        L list -> showRoundList $ map shows list
        A a -> shows a
        C a -> shows a

instance Show Atom where
    showsPrec _ a = case a of
        Str s -> shows s
        Sym s -> showString s
        Num n -> shows n

stringEscapable :: Char -> Char -> P String
stringEscapable marker escape = paren marker marker $ many chr
    where
    chr = quotedChr <|> normalChr
    normalChr = charExcept [marker]
    quotedChr = fmap special $ char escape *> anyChar
    special 'n' = '\n'
    special 't' = '\t'
    special c = c

sexp :: P (GS a)
sexp = ws *> sexp1
    where
    sexp1 :: P (GS a)
    sexp1 = atom <|> quote <|> list
    
    quote = fmap wrap $ do
        char '\''
        sexp
        where
        wrap s = L [A (Sym "quote"), s]
    
    atom = fmap A $ num <|> str <|> sym
        where
        num = fmap Num $ number <|> paren '<' '>' expr
        str = fmap Str $ stringEscapable '"' '\\'
        sym = fmap Sym $ name <|> stringEscapable '|' '\\'
        
        name = liftA2 (:) first (many rest)
        first = charExcept $ prohibited ++ "0123456789."
        rest = charExcept prohibited
        prohibited = " \t\n\\,'\"()<>[]|"
    
    list = fmap L $ paren '(' ')' $ do
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

parseSexp :: String -> Either String (GS a, String)
parseSexp = runP sexp

parseManySexp :: String -> Either String ([GS a], String)
parseManySexp = runP (many sexp)

instance Read (GS a) where
    readsPrec _ str = case parseSexp str of
        Right v -> [v]
        Left e -> error e

--

type Frame = [(String, V)]
addFrame name value frame = (name, value) : frame
makeFrame kvs = kvs :: Frame
findFrame name frame = lookup name frame

type Env = [Frame]
addEnv frame env = frame : env
findEnv name env = msum $ map (findFrame name) env

newtype I a = I (ErrorT String (StateT Env IO) a) deriving (Monad, Functor, MonadIO)

sandbox :: (Monad m) => StateT s m a -> StateT s m a
sandbox m = StateT $ \s -> do
    (a, _) <- runStateT m s
    return (a, s)

withFrame :: Frame -> I a -> I a
withFrame frame (I m) = I $ ErrorT $ sandbox $ withStateT (frame:) (runErrorT m)

withEnv :: Env -> I a -> I a
withEnv env (I m) = I $ ErrorT $ sandbox $ withStateT (const env) (runErrorT m)

defineEnv :: String -> V -> Env -> Env
defineEnv name value [] = [makeFrame [(name, value)]]
defineEnv name value (f:fs) = addFrame name value f : fs

lookupName :: String -> I V
lookupName name = I $ do
    r <- gets $ findEnv name
    maybe (fail $ "variable |" ++ name ++ "| not found") return r

runI :: Env -> I a -> IO (Either String a, Env)
runI env (I i) = runStateT (runErrorT i) env

evalI :: Env -> I a -> IO (Either String a)
evalI env (I i) = evalStateT (runErrorT i) env

type S = GS V

data V
    = S S
    | U String
    | F Env (Either [String] String) [S]
    | Prim String Prim

data Prim
    = Subr ([V] -> I V)
    | Syntax ([S] -> I V)

instance Show V where
    showsPrec _ v = case v of
        S s -> shows s
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
eval s@(L []) = return $ S s
eval (L (x:xs)) = do
    x <- eval x
    apply x xs
eval (A (Sym a)) = do
    lookupName a
eval s@(A _) = return $ S s

apply :: V -> [S] -> I V
apply (U m) _ = fail $ "can't apply undefined: " ++ show m
apply v@(F env argspec f) args = do
    values <- mapM eval args
    withEnv (flip addEnv env $ makeFrame $ bind argspec values) $ do
        evalBegin f
    where
    bind (Left argnames) values = zip argnames values
    bind (Right argname) values = [(argname, listToV values)]
apply (Prim _ p) args = applyPrim p args
apply (S s) _ = fail $ "can't apply " ++ show s

listToV :: [V] -> V
listToV vs = S $ L $ map vtoS vs

vtoS :: V -> S
vtoS (S s) = s
vtoS e = C e
    where
    pull (S s) = s
    pull e = C e

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

evalDefine [A (Sym name), exp] = do
    val <- eval exp
    I $ modify $ defineEnv name val
    return $ U $ "defined '" ++ name ++ "'"

isSymbol :: S -> Bool
isSymbol (A (Sym _)) = True
isSymbol _ = False

evalLambda (argspec:body) = do
    case argspec of
        L names -> do
            let args = [name | A (Sym name) <- names]
            unless (length args == length names) $ do
                fail $ "invalid arg spec"
            env <- I get
            return $ F env (Left args) body
        A (Sym name) -> do
            env <- I get
            return $ F env (Right name) body
        _ -> fail "invalid arg spec"

----

ii :: String -> IO (Either String V)
ii str = case parseManySexp str of
    Left e -> return $ Left $ "Parse error:" ++ e
    Right (s, _) -> evalI [prims] $ evalBegin s
    where
    prims = map (\(name, prim) -> (name, Prim name prim)) registerPrims
    
    registerPrims = flip execState [] list
    subr name f = add (name, Subr f)
    syntax name s = add (name, Syntax s)
    add prim = modify (prim:)
    list = do
        subr "p" $ \vs -> do
            liftIO $ mapM_ print vs
            return $ U "print"
        
        syntax "begin" evalBegin
        syntax "define" evalDefine
        syntax "lambda" evalLambda
        syntax "quote" $ \[e] -> return $ S e
