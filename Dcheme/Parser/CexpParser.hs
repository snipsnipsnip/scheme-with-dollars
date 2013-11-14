module Dcheme.Parser.CexpParser
( module Dcheme.Parser.SexpParser
, parseManyCexp
) where

import Control.Monad.State
import Data.List
import Debug.Trace
import Dcheme.Parser.ExprParser
import Dcheme.Parser.SexpParser
import Dcheme.Parser.ParserCombinator

parseManyCexp :: String -> Either String ([S], String)
parseManyCexp = runP $ fmap cToS $ cexp $ char '$'

type Ctoken = (Int, Maybe S)
type Cexp = [Ctoken]

cexp :: MonadParser p => p a -> p Cexp
cexp p = fmap concat $ many $ do
    whitespace
    quote <|> do
      fmap return $ liftA2 (,) (fmap (2 *) pos) (colon <|> s)
    where
    colon = p $> Nothing
    s = fmap Just sexp
    pos = fmap fst getCaret
    quote = do
      p <- pos
      char '\'' $> [(p * 2, Nothing), (p * 2 + 1, Just $ S $ Left $ Sym "quote")]

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
        return $ reverse s
    
    bag :: Ctoken -> M ()
    bag (indent, elem) = do
        level <- getLevel
        let ord = compare indent level
        case (elem, ord) of
            (Just s, GT) -> add s
            (Just s, EQ) -> shift >> add s
            (Just s, LT) -> shiftTo (indent - 1) >> add s
            (Nothing, GT) -> unshift indent
            (Nothing, EQ) -> shift >> unshift indent
            (Nothing, LT) -> shiftTo (indent - 1) >> unshift indent
        
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
        
    add :: S -> M ()
    add s = do
        (l, list):rest <- get
        put $ (l, s:list):rest

untilM :: (Monad m) => (a -> Bool) -> m a -> m b -> m ()
untilM pred var action = do
    curr <- var
    when (pred curr) $ do
        action
        untilM pred var action

hoge :: Cexp
hoge = c
    where
    Right (c,_) = runP (cexp $ char ':') $ intercalate "\n"
        [ ": define filter"
        , "  : Y"
        , "    : ^ : filter"
        , "      : ^ : f xs"
        , "        : if : null xs"
        , "          xs"
        , "          : if : f : car xs"
        , "            : cons : car xs"
        , "                   : filter f : cdr xs"
        , "            : filter f : cdr xs"
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
