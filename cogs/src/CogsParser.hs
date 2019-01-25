{-# LANGUAGE TupleSections #-}

module CogsParser (cogsParser) where

import           Parser
import           Types

import           Control.Monad (guard, void)
import           Data.Char     (isDigit)
import           Data.Maybe    (fromMaybe, listToMaybe)

-- | Main parser that parses the entire file
cogsParser :: Parser Board
cogsParser = do
    width <- parseKeyContainer "width" parseNumber
    height <- parseKeyContainer "height" parseNumber

    let coordinateParser = parseCoordinate width height

    let sourcevalidator = resolve width height []
    let sourceparser = parseKeyContainer "sources" (parseList coordinateParser)
    sources <- validate sourceparser sourcevalidator
    let sinkvalidator = resolve width height sources
    let sinkparser = parseKeyContainer "sinks"  (parseList coordinateParser)
    sinks   <- validate sinkparser sinkvalidator
    tiles   <- parseKeyContainer "tiles" (parseList (parseTile coordinateParser))
    guard (unique (fmap getTilePosition tiles)) <|> throwError "two or more tiles at the same location"
    _ <- optional whitespace
    let board = Board width height (fmap (uncurry Source) sources) (fmap (uncurry Sink) sinks) tiles
    return board

-- | Checks that the parsed result properly transforms
validate :: Parser a -> (a -> Maybe b) -> Parser b
validate maybeParser validator = do
    result <- maybeParser
    case validator result of
        Nothing -> empty
        (Just val) -> return val

-- | Puts a list of sinks/sources in it's place/direction given a list of previous sources/sinks
resolve :: Int -> Int -> [(Coordinate, Direction)] -> [Coordinate] -> Maybe [(Coordinate, Direction)]
resolve _ _ _ [] = Just []
resolve w h used (coord:todo) = case newEdge of
        Nothing -> Nothing
        (Just edge) -> fmap (edge:) (resolve w h (edge:used) todo)
    where newEdge = listToMaybe $ filter (`notElem` used) (possibleEdgeLocations w h coord)

-- | Gives a list of possible placements for sinks/sources on a certain coordinate
possibleEdgeLocations :: Int -> Int -> Coordinate -> [(Coordinate, Direction)]
possibleEdgeLocations width height coordinate = filter (not . inRange width height . fst) neighbours
    where neighbours = fmap (\dir -> (coordinate + dir, oppositeDirection dir)) allDirections

-- | Parses a coordinate and validates that it is in the board
parseCoordinate :: Int -> Int -> Parser Coordinate
parseCoordinate maxwidth maxheight = do
    _ <- character '('
    f <- parseNumber
    -- I would use separatingComma, but can't because coordinates only allows horizontal whitespace
    _ <- optional horizontalWhitespace
    _ <- character ','
    _ <- horizontalWhitespace
    s <- parseNumber
    _ <- character ')'
    let coordinate = (f, s)
    guard (inRange maxwidth maxheight coordinate) <|> throwError ("coordinate not in board: " ++ show coordinate)
    return coordinate

-- | Takes a parser and transforms it into the same parser, but with a comma separating before it
separatingComma :: Parser a -> Parser a
separatingComma parser = do
    _ <- optional whitespace
    _ <- character ','
    _ <- whitespace
    parser

-- | Given a coordinate parser, parses a tile
parseTile :: Parser Coordinate -> Parser Tile
parseTile coordinateParser = do
    _ <- character '{'
    _ <- optional whitespace
    pos <- listKeyParser "pos" coordinateParser
    fixed <- separatingComma $ listKeyParser "fixed" parseFixedness
    -- if there is no default key, the default is "not connected"
    defaultConnect <- separatingComma (listKeyParser "default" parseOpenClosed) <|> return False
    connections <- many $ separatingComma (directionKeyParser parseOpenClosed)
    guard (unique (fmap fst connections)) <|> throwError "duplicate direction found in tile"
    let allConnections = fmap fst $ filter snd $ fmap (\dir -> (dir, fromMaybe defaultConnect $ lookup dir connections)) allDirections
    _ <- optional whitespace
    _ <- character '}'
    return $ Tile fixed allConnections pos

-- | Are all elements in the list unique?
unique :: Eq a => [a] -> Bool
unique = unique' []
    where unique' _    []     = True
          unique' seen (x:xs) = (x `notElem` seen) && unique' (x : seen) xs

-- | Given a value parser, parses a key-value pair where the key is a direction
directionKeyParser :: Parser a -> Parser (Direction, a)
directionKeyParser valueparser = do
    let dirs = [("north", north), ("east", east), ("south", south), ("west", west)]
    useFirstParser $ fmap (\(keyname, dir) -> listKeyParser keyname (fmap (dir, ) valueparser)) dirs

-- | Parses whether a pipe is open/closed
parseOpenClosed :: Parser Bool
parseOpenClosed = (string "open" >> return True) <|> (string "closed" >> return False) <|> throwError "expected 'open' or 'closed'"

-- | Parses whether a tile is fixed or moveable
parseFixedness :: Parser Bool
parseFixedness = (string "yes" >> return False) <|> (string "no" >> return True) <|> throwError "expected 'yes' or 'no'"

-- | Given a parser, parses a value from a key in a list
listKeyParser :: String -> Parser a -> Parser a
listKeyParser key valueparser = do
    _ <- string key <|> throwError ("expected list key: " ++ key)
    _ <- character ':'
    _ <- horizontalWhitespace
    valueparser

-- | Parses some horizontal whitespace
horizontalWhitespace :: Parser ()
horizontalWhitespace = void $ some $ character ' ' <|> character '\t'

-- | Parses one newline
newline :: Parser ()
newline = void $ windows <|> mac <|> linux
    where windows = character '\r' >> character '\n'
          linux   = character '\n'
          mac     = character '\r'

-- | Parses some newlines or horizontalWhitespace
whitespace :: Parser ()
whitespace = void $ some (newline <|> horizontalWhitespace)

-- | Given a parses, parses a list of those things
parseList :: Parser a -> Parser [a]
parseList elemparser = do
    _ <- character '{'
    _ <- optional whitespace
    -- Lists always contain at least one element
    f <- elemparser
    l <- many (separatingComma elemparser)
    _ <- optional whitespace
    _ <- character '}'
    return $ f:l

-- | Parses a natural number
parseNumber :: Parser Int
parseNumber = do
    digits <- some $ sat isDigit
    return $ read digits

-- | Given a key (string) and a value parser, parses the key-value pair
parseKeyContainer :: String -> Parser a -> Parser a
parseKeyContainer key valueparser = do
    _ <- string key <|> throwError ("expected main key " ++ show key)
    _ <- character ':'
    horizontalWhitespace
    retval <- valueparser
    _ <- newline
    return retval
