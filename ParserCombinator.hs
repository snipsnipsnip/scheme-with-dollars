{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ParserCombinator
( P ()
, MonadParser (..)
, char
, charExcept
, ($>)
, asum
, paren
, runP
, evalP
, eatUntil
, escapableString
, module Control.Applicative
, module Control.Monad
) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative

class (Monad p, Applicative p, Alternative p) => MonadParser p where
    anyChar :: p Char
    getCaret :: p (Int, Int)

instance MonadParser p => MonadParser (StateT s p) where
    anyChar = lift anyChar
    getCaret = lift getCaret

instance Monad m => Applicative (StateT s m) where
    pure = return
    (<*>) = ap

instance (Monad m, Alternative m) => Alternative (StateT s m) where
    empty = StateT $ \_ -> empty
    StateT a <|> StateT b = StateT $ \s -> a s <|> b s

---

newtype P a = P (StateT (String, Int, Int) (Either String) a)
    deriving (Monad, Functor)

instance Applicative P where
    pure = return
    (<*>) = ap

instance Alternative P where
    empty = fail "P: empty"
    P a <|> P b = P $ a `catchError` const b

instance MonadPlus P where
    mzero = empty
    mplus = (<|>)

instance MonadParser P where
    getCaret = P $ gets (\(_, caret, line) -> (caret, line))
    anyChar = anyCharP

runP :: P a -> String -> Either String (a, String)
runP (P p) s = fmap cut $ runStateT p (s, 0, 0)
    where
    cut (a,(s,_,_)) = (a, s)

evalP :: P a -> String -> Either String a
evalP p s = fmap fst $ runP p s

anyCharP :: P Char
anyCharP = P $ do
    (str, caret, line) <- get
    case str of
        d:s -> do
            put $ case d of
                '\n' -> (s, 0, line + 1)
                _ -> (s, caret + 1, line)
            return d
        _ -> do
            fail "eof"

char :: MonadParser p => Char -> p Char
char c = do
    d <- anyChar
    unless (c == d) $ fail $ ($ "") $
        showString "expected char " .
        showChar c .
        showString ", but got " .
        showChar d
    return d

charExcept :: MonadParser p => String -> p Char
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

paren :: MonadParser p => Char -> Char -> p a -> p a
paren open close p = char open *> p <* char close

eatUntil :: MonadParser p => String -> p ()
eatUntil str = loop str
    where
    loop "" = return ()
    loop (c:cs) = do
        d <- anyChar
        if c == d then loop cs else loop str

escapableString :: MonadParser p => Char -> Char -> p String
escapableString marker escape = paren marker marker $ many chr
    where
    chr = quotedChr <|> normalChr
    normalChr = charExcept [marker]
    quotedChr = fmap special $ char escape *> anyChar
    special 'n' = '\n'
    special 't' = '\t'
    special c = c

asum :: Alternative f => [f a] -> f a
asum = foldl (<|>) empty
