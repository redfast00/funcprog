{-# LANGUAGE LambdaCase #-}


module Parser (Parser, character, string, some, many, optional, empty, sat, useFirstParser, throwError, parseStatement, (<|>)) where


-- parser overgenomen en aangepast van https://github.com/redfast00/stutter
-- https://github.com/redfast00/stutter heb ik zelf geschreven

import           Control.Applicative (Alternative (..), many, optional, some,
                                      (<|>))
import           Control.Monad       (MonadPlus (..), ap, liftM)
import           Data.Either         (isLeft, isRight)

type ErrorMessage = String

newtype Parser a = Parser (String -> [(Either ErrorMessage a, String)])

parse :: Parser a -> String -> [(Either ErrorMessage a, String)]
parse (Parser p) = p

parseStatement :: String -> Parser a -> Either ErrorMessage a
parseStatement a parser = case parse parser a of
    [(Right x,"")]  -> Right x
    [(Left x, _)]   -> Left x
    [(_,rest)]      -> Left $ "parsing failed, near: " ++ take 10 rest
    []              -> Left "Failed to parse"
    (_, _):(_, _):_ -> Left "Ambiguous parse"

sat  :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else mzero}

character :: Char -> Parser Char
character c = sat (==c)

string :: String -> Parser String
string s = do
    mapM_ character s
    return s

item :: Parser Char
item  = Parser (\case
                    ""     -> []
                    (c:cs) -> [(Right c,cs)])

useFirstParser :: [Parser a] -> Parser a
useFirstParser = foldl1 (<|>)

throwError :: ErrorMessage -> Parser a
throwError msg = Parser (\cs -> [(Left msg, cs)])

instance Monad Parser where
    return a = Parser (\cs -> [(Right a,cs)])
    p >>= f  = Parser (\cs ->
                        let firstParsed = parse p cs -- List of (a, cs')
                            succeeded   = (\(Right x, cs'') -> (x, cs'')) <$> filter (isRight . fst) firstParsed
                            failed      = (\(Left x,  cs'') -> (x, cs'')) <$> filter (isLeft . fst) firstParsed
                        in case succeeded of
                            [] -> case failed of
                                []             -> []
                                ((msg, cs'):_) -> [(Left msg, cs')]
                            _  -> concat [parse (f a) cs' | (a, cs') <- succeeded]
                      )

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance MonadPlus Parser where
    mzero = Parser (const [])
    m1 `mplus` m2 = Parser (\s -> parse m1 s ++ parse m2 s)

bestCase :: [(Either a b, c)] -> [(Either a b, c)]
bestCase [] = []
bestCase ((Right v, cs) : _) = [(Right v, cs)]
bestCase (firsterror@(Left _, _) : xs) = case bestCase xs of
    []            -> [firsterror]
    [(Left _, _)] -> [firsterror]
    other         -> other

instance Alternative Parser where
    -- When there are multiple parsing options, select the first one without error
    p1 <|> p2 = Parser $ \s -> bestCase $ parse (mplus p1 p2) s
    empty = mzero
