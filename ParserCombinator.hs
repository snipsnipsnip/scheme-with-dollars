{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ParserCombinator
( P ()
, anyChar
, char
, charExcept
, ($>)
, paren
, runP
, evalP
, eatUntil
, escapableString
, getCaret
, module Control.Applicative
, module Control.Monad
) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative

newtype P a = P (StateT (String, Int, Int) (Either String) a)
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

getCaret :: P (Int, Int)
getCaret = P $ gets (\(_, caret, line) -> (caret, line))

runP :: P a -> String -> Either String (a, String)
runP (P p) s = fmap cut $ runStateT p (s, 0, 0)
    where
    cut (a,(s,_,_)) = (a, s)

evalP :: P a -> String -> Either String a
evalP p s = fmap fst $ runP p s

anyChar :: P Char
anyChar = P $ do
    (str, caret, line) <- get
    case str of
        d:s -> do
            put $ case d of
                '\n' -> (s, 0, line + 1)
                _ -> (s, caret + 1, line)
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

eatUntil :: String -> P ()
eatUntil str = loop str
    where
    loop "" = return ()
    loop (c:cs) = do
        d <- anyChar
        if c == d then loop cs else loop str

escapableString :: Char -> Char -> P String
escapableString marker escape = paren marker marker $ many chr
    where
    chr = quotedChr <|> normalChr
    normalChr = charExcept [marker]
    quotedChr = fmap special $ char escape *> anyChar
    special 'n' = '\n'
    special 't' = '\t'
    special c = c
