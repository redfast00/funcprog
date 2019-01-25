{-# LANGUAGE LambdaCase #-}

module Parser (Parser(..), executeParser, character, useFirstParser) where

import           Control.Applicative
import           Control.Monad

newtype Parser a = Parser (String -> [(a, String)])

parse, apply :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

apply = parse

instance Monad Parser where
    return a = Parser (\cs -> [(a,cs)])
    p >>= f  = Parser (\cs -> concat [parse (f a) cs' |
                                (a, cs') <- parse p cs])

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance MonadPlus Parser where
    mzero = Parser (const [])
    m1 `mplus` m2 = Parser (\s -> apply m1 s ++ apply m2 s)

instance Alternative Parser where
    -- When there are multiple parsing options, select the first one
    p1 <|> p2 = Parser $ \s -> case apply (mplus p1 p2) s of
                                []    -> []
                                (x:_) -> [x]
    empty = mzero

executeParser :: Parser a -> String -> Either String a
executeParser parser a = case parse parser a of
    [(x,"")] -> Right x
    _        -> Left "parsing failed"

item :: Parser Char
item  = Parser (\case
                    ""     -> []
                    (c:cs) -> [(c,cs)])

sat :: (Char -> Bool) -> Parser Char
sat predicate = do
    c <- item
    if predicate c then return c else mzero

character :: Char -> Parser Char
character c = sat (==c)

useFirstParser :: [Parser a] -> Parser a
useFirstParser = foldl1 (<|>)
