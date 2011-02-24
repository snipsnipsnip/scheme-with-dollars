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

instance Show S where
    showsPrec _ s = case s of
        L list -> showParen True $ foldl (.) id $ intersperse (showChar ' ') $ map shows list
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
        
        name = liftA2 (:) first (some rest)
        first = charExcept $ prohibited ++ "0123456789."
        rest = charExcept prohibited
        prohibited = " \t\n\\,'\"()<>[]|"
    
    list = fmap L $ paren '(' ')' $ do
        whitespace
        list1 <* whitespace <|> pure []
        where
        list1 = do
            liftA2 (:) sexp1 $ many $ whitespace1 *> sexp1

instance Read S where
    readsPrec _ str = case runP sexp str of
        Right v -> [v]
        Left e -> error e

--

type Frame = [(String, V)]
addFrame name value frame = (name, value) : frame
findFrame name frame = lookup name frame

type Env = [Frame]
addEnv frame env = frame : env
findEnv name env = msum $ map (findFrame name) env

newtype I a = I (ErrorT String (StateT Env IO) a) deriving (Monad, Functor)

withFrame :: Frame -> I a -> I a
withFrame frame (I m) = I $ ErrorT $ withStateT (frame:) (runErrorT m)

lookupName :: String -> I V
lookupName name = I $ do
    r <- gets $ findEnv name
    maybe (fail $ "variable |" ++ name ++ "| not found") return r

runI :: Env -> I a -> IO (Either String a, Env)
runI env (I i) = runStateT (runErrorT i) env

evalI :: Env -> I a -> IO (Either String a)
evalI env (I i) = evalStateT (runErrorT i) env

ii :: I a -> IO (Either String a)
ii = evalI []

data V
    = S S
    | U String
    | F Env [String] [S]
    deriving (Show, Eq)

eval :: S -> I V
eval (Q s) = return $ S s
eval (L xs) = do
    r:rs <- mapM eval xs
    apply r rs
eval (A (Sym a)) = do
    lookupName a
eval s = return $ S s

apply :: V -> [V] -> I V
apply (U m) _ = fail $ ($ "") $ showString "can't apply undefined: "  . shows m
apply (F env argnames f) args = do
    return $ S $ A $ Sym "eval'd"
apply (S s) _ = fail $ ($ "") $ showString "can't apply "  . shows s
