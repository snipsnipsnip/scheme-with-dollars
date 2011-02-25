module CexpParser
( module SexpParser
, parseIndentedSexp
, parseManyIndentedSexp
) where

import Control.Monad.State
import ExprParser
import ParserCombinator
import List
import SexpParser

parseIndentedSexp :: String -> Either String (S, String)
parseIndentedSexp = undefined

parseManyIndentedSexp :: String -> Either String ([S], String)
parseManyIndentedSexp = undefined

{-
えーと、構造は

プログラム = 行*
行 = スペース* (行要素)* '\n'
行要素 = スペース* 要素
要素 = コロン | S式
スペース = ' ' | '\t'
コロン = ':'

なんだけど、行にまたがってS式があるからどうすればいいのか
-}

type Cexp = [(Int, Maybe Atom)]

cexp :: P Cexp
cexp = many (ws *> liftA2 (,) pos (char ':' $> Nothing <|> fmap Just parseAtom))
    where
    ws = many (char ' ' <|> char '\t')
    pos = fmap fst getCaret

hmm cexp = dive 0 cexp []
    where
    dive _ [] = (0, [], [])
    dive p ((q, Nothing):ts) = case compare p q of
        LT -> s : dive q rest
        EQ -> 
        GT ->
        where
        (r, s, rest) = dive q ts
    dive p ((q, Just atom):ts)
        | p < q
        |
