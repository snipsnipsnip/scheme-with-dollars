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

data S
    = L [S]
    | Q S
    | A Atom
    deriving Eq

data Atom
    = Str String
    | Sym String
    | Num Double
    deriving Eq

showRoundList :: [ShowS] -> ShowS
showRoundList list =
    showParen True $ foldl (.) id $ intersperse (showChar ' ') list

instance Show S where
    showsPrec _ s = case s of
        L list -> showRoundList $ map shows list
        Q s -> showChar '\'' . shows s
        A a -> shows a

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

sexp :: P S
sexp = whitespace *> sexp1
    where
    sexp1 = atom <|> quote <|> list
    
    quote = fmap Q $ do
        char '\''
        sexp
    
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
        whitespace
        list1 <* whitespace <|> pure []
        where
        list1 = do
            liftA2 (:) sexp1 $ many $ whitespace1 *> sexp1

parseSexp = runP sexp

instance Read S where
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

withFrame :: Frame -> I a -> I a
withFrame frame (I m) = I $ ErrorT $ withStateT (frame:) (runErrorT m)

withEnv :: Env -> I a -> I a
withEnv env (I m) = I $ ErrorT $ withStateT (const env) (runErrorT m)

lookupName :: String -> I V
lookupName name = I $ do
    r <- gets $ findEnv name
    maybe (fail $ "variable |" ++ name ++ "| not found") return r

runI :: Env -> I a -> IO (Either String a, Env)
runI env (I i) = runStateT (runErrorT i) env

evalI :: Env -> I a -> IO (Either String a)
evalI env (I i) = evalStateT (runErrorT i) env

data V
    = S S
    | U String
    | F Env [String] [S]
    | Prim String Prim

data Prim
    = Subr ([V] -> I V)
    | Syntax ([S] -> I V)

instance Show V where
    showsPrec _ v = case v of
        S s -> shows s
        U s -> val "undef" $ showString s
        F env args body -> val "func" $ showRoundList (map showString args)
        Prim name prim -> val (primName prim) $ showString name
        where
        val name s = showString "#<" . showString name . showChar ' ' . s . showChar '>'
        primName (Subr _) = "subr"
        primName (Syntax _) = "syntax"

eval :: S -> I V
eval (Q s) = return $ S s
eval s@(L []) = return $ S s
eval (L (x:xs)) = do
    x <- eval x
    apply x xs
eval (A (Sym a)) = do
    lookupName a
eval s@(A _) = return $ S s

apply :: V -> [S] -> I V
apply (U m) _ = fail $ "can't apply undefined: " ++ show m
apply (F env argnames f) args = do
    values <- mapM eval args
    withEnv env $ do
        withFrame (makeFrame $ zip argnames values) $ do
            e <- I get
            evalBegin f
apply (Prim _ p) args = applyPrim p args
apply (S s) _ = fail $ "can't apply " ++ show s

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

----

ii :: String -> IO (Either String V)
ii str = case parseSexp str of
    Left e -> return $ Left e
    Right (s, rest) -> evalI [prims] $ eval s
    where
    prims = map (\p@(Prim name _) -> (name, p))
        [ Prim "p" $ Subr $ \vs -> do
            liftIO $ mapM_ print vs
            return $ U "print"
        , Prim "begin" $ Syntax evalBegin
        ]
