module SexpParser
( S (..)
, Atom (..)
, showRoundList
, parseSexp
, parseManySexp
) where

import ExprParser
import ParserCombinator
import List

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
    showsPrec _ (S (Left a)) = shows a
    showsPrec _ (S (Right l)) = shows l
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
parseManySexp = runP (some sexp)

instance Read S where
    readsPrec _ str = case parseSexp str of
        Right v -> [v]
        Left e -> error e
