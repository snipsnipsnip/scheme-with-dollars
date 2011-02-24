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
    unless (c == d) $ fail $ "expected " ++ show c ++ ", but got " ++ show d
    return d

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

newtype S = S (Either [S] Atom)
type Atom = Either String Double

instance Show S where
    showsPrec _ (S s) = case s of
        Left list -> showParen True $ foldl (.) id $ intersperse (showChar ' ') $ map shows list
        Right (Left atom) -> shows atom
        Right (Right atom) -> shows atom

sexp :: P S
sexp = whitespace *> sexp1
    where
    sexp1 = atom <|> list
    atom = fmap (S . Right) $ do
        whitespace
        num <|> str
    num = fmap Right $ number <|> paren '<' '>' expr
    str = fmap Left $ paren '"' '"' $ many $ do
        char '\\' *> anyChar <|> do
          c <- anyChar
          guard (c /= '"')
          return c
    list = fmap (S . Left) $ paren '(' ')' $ do
        whitespace
        list1 <* whitespace <|> pure []
    list1 = do
        liftA2 (:) sexp1 $ many $ whitespace1 *> sexp1

