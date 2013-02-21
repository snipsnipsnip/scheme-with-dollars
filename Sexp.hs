module Sexp
( S (..)
, Atom (..)
, showRoundList
) where

import Data.List

newtype S = S (Either Atom [S])
    deriving (Eq)

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
