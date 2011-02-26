module CexpParser
( module SexpParser
, parseIndentedSexp
, parseManyIndentedSexp
) where

import Control.Monad.State
import ExprParser
import ParserCombinator
import Data.List
import qualified Data.IntMap as I
import SexpParser
import Debug.Trace

parseIndentedSexp :: String -> Either String (S, String)
parseIndentedSexp = undefined

parseManyIndentedSexp :: String -> Either String ([S], String)
parseManyIndentedSexp = undefined

type Cexp = [(Int, Maybe Atom)]

cexp :: P Cexp
cexp = many $ do
    whitespace
    liftA2 (,) pos (colon <|> atom)
    where
    colon = char ':' $> Nothing
    atom = fmap Just parseAtom
    pos = fmap fst getCaret

type M a = State [(Int, [S])] a

cToS :: Cexp -> [S]
cToS cexp = evalState reduce [(-1, [])]
    where
    reduce :: M [S]
    reduce = do
        mapM_ bag cexp
        x <- get
        shiftTo (-1)
        [(-1, s)] <- get
        return s
    
    bag :: (Int, Maybe Atom) -> M ()
    bag (indent, elem) = do
        level <- getLevel
        let ord = compare level indent
        case (elem, ord) of
            (Just atom, LT) -> addAtom atom
            (Just atom, EQ) -> shift >> addAtom atom
            (Just atom, GT) -> shiftTo (indent - 1) >> addAtom atom
            (Nothing, LT) -> unshift indent
            (Nothing, EQ) -> shift >> unshift indent
            (Nothing, GT) -> shiftTo (indent - 1) >> unshift indent
        
    getLevel :: M Int
    getLevel = gets (fst . head)

    shift :: M ()
    shift = do
        (r,list):(l,next):rest <- get
        put $ (l, S (Right $ reverse list):next) : rest

    unshift :: Int -> M ()
    unshift level = modify ((level, []) :)

    shiftTo :: Int -> M ()
    shiftTo level = untilM (level <) getLevel shift
        
    addAtom :: Atom -> M ()
    addAtom atom = do
        (l, list):rest <- get
        put $ (l, S (Left atom):list):rest

untilM :: (Monad m) => (a -> Bool) -> m a -> m b -> m ()
untilM pred var action = do
    curr <- var
    when (pred curr) $ do
        action
        untilM pred var action

hoge :: Cexp
hoge = c
    where
    Right (c,_) = runP cexp $ intercalate "\n"
        [ ": define filter"
        , "  : Y"
        , "    : ^ : filter"
        , "      : ^ : f xs"
        , "        : if : null xs"
        , "          xs"
        , "          : if : f : car xs"
        , "            : cons : car xs"
        , "                   : filter f : cdr xs"
        , "            filter f : cdr xs"
        ]

fuga :: S
fuga = s
    where
    Right (s, _) = parseSexp $ intercalate "\n"
        [ "(define filter"
        , "  (Y"
        , "    (^ (filter)"
        , "      (^ (f xs)"
        , "        (if (null xs)"
        , "          xs"
        , "          (if (f (car xs))"
        , "            (cons (car xs) (filter f (cdr xs)))"
        , "            (filter f (cdr xs))))))))"
        ]

test :: Bool
test = cToS hoge == [fuga]
