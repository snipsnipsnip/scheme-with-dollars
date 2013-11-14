module Dcheme.Parser.ExprParser
( number
, whitespace
, whitespace1
) where

import Dcheme.Parser.ParserCombinator

expr :: MonadParser p => p Double
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

whitechar :: MonadParser p => p Char
whitechar = asum $ map char " \t\n"

whitespace :: MonadParser p => p String
whitespace = many whitechar

whitespace1 :: MonadParser p => p String
whitespace1 = some whitechar

number :: MonadParser p => p Double
number = do
    fmap read $ some $ asum $ map char ['0'..'9']

test :: Bool
test = runP expr "1 + 1 * (6/9 )* 3 + 4" == Right (7, "")
