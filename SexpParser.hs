module SexpParser
( module Sexp
, parseSexp
, parseManySexp
, parseAtom
, parseList
, sexp
) where

import ExprParser
import ParserCombinator
import Sexp

sexp :: MonadParser p => p S
sexp = ws *> sexp1

sexp1 :: MonadParser p => p S
sexp1 = fmap S (atom <|> quote <|> list)
    where
    quote = fmap wrap $ do
        char '\''
        sexp
        where
        wrap s = Right [S $ Left $ Sym "quote", s]
    
    atom = fmap Left parseAtom
    
    list = fmap Right parseList
    
ws, ws1, wsaft :: MonadParser p => p ()
ws = whitespace >> wsaft
ws1 = whitespace1 >> wsaft
wsaft = comment >> whitespace $> ()
    where
    comment = mapM_ char "#|" *> eatUntil "|#" <|> return ()

parseAtom :: MonadParser p => p Atom
parseAtom = str <|> sharp <|> num <|> sym 
    where
    num = fmap Num $ number
    str = fmap Str $ escapableString '"' '\\'
    sym = fmap Sym $ name <|> escapableString '|' '\\'
    sharp = char '#' >> true <|> false <|> chr
    true = char 't' $> Bool True
    false = char 'f' $> Bool False
    chr = char '\\' *> fmap Chr (special <|> anyChar)
    special = mapM_ char "newline" $> '\n' <|> mapM_ char "space" $> ' '
    
    name = liftA2 (:) first (many rest)
    first = charExcept $ prohibited ++ ".#"
    rest = charExcept prohibited
    prohibited = " \t\n\\,'\"()[]|"

parseList :: MonadParser p => p [S]
parseList = paren '(' ')' $ do
    ws
    list1 <* ws <|> pure []
    where
    list1 = do
        liftA2 (:) sexp1 $ many $ ws1 *> sexp1

parseSexp :: String -> Either String (S, String)
parseSexp = runP sexp

parseManySexp :: String -> Either String ([S], String)
parseManySexp = runP (some sexp)

instance Read S where
    readsPrec _ str = case parseSexp str of
        Right v -> [v]
        Left e -> error e
