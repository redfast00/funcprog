module BoardParser where

import           Parser
import           Types

import           Control.Applicative (many)
import qualified Data.Map.Strict     as Map

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

parseBoard :: Parser Board
parseBoard = do
    board <- many parseBoardLine
    return $ list2board board

tileMap :: [[(Tile, Maybe Entity)]] -> Map.Map Coordinate Tile
tileMap list = Map.fromList $ do
    (y, row) <- enumerate list
    (x, (tile, _)) <- enumerate row
    return ((y, x), tile)

sausageList :: [[(Tile, Maybe Entity)]] -> [Sausage]
sausageList list = do
    (y, row) <- enumerate list
    (x, (_, entity)) <- enumerate row
    case entity of
        (Just (SausageEntity North)) -> return $ Sausage first second
            where first  = Half Raw Raw North (y, x)
                  second = Half Raw Raw South (y + 1, x)
        (Just (SausageEntity West))  -> return $ Sausage first second
            where first  = Half Raw Raw West (y, x)
                  second = Half Raw Raw East (y, x + 1)
        _ -> mempty

findPlayer :: [[(Tile, Maybe Entity)]] -> Player
findPlayer list = head $ do
    (y, row) <- enumerate list
    (x, (_, entity)) <- enumerate row
    case entity of
        (Just (PlayerEntity dir)) -> return $ Player (y, x) dir
        _ -> mempty

list2board :: [[(Tile, Maybe Entity)]] -> Board
list2board list = Board (tileMap list) parsedSausages (findPlayer list) (length parsedSausages)
    where parsedSausages = sausageList list


parseTile :: Parser Tile
parseTile = useFirstParser [grass, water, grill]
    where grass = character '+' >> return Grass
          water = character '~' >> return Water
          grill = character '#' >> return Grill


parseDirection :: Parser Direction
parseDirection = useFirstParser [north, east, south, west]
    where north = character 'N' >> return North
          east  = character 'E' >> return East
          south = character 'S' >> return South
          west  = character 'W' >> return West

parsePlayer :: Parser Entity
parsePlayer = PlayerEntity <$> parseDirection

parseSausageEntity :: Parser Entity
parseSausageEntity = SausageEntity <$> useFirstParser [north, east, south, west]
    where north = character '∩' >> return North
          east  = character '⊃' >> return East
          south = character '∪' >> return South
          west  = character '⊂' >> return West


parseEntity :: Parser (Maybe Entity)
parseEntity = useFirstParser [sausagepart, player, ignoreFork, ignoreEmpty]
    where sausagepart = Just <$> parseSausageEntity
          player      = Just <$> parsePlayer
          ignoreFork  = character 'x' >> return Nothing
          ignoreEmpty = character ' ' >> return Nothing

parseBoardLine :: Parser [(Tile, Maybe Entity)]
parseBoardLine = do
    result <- many $ do
        tile <- parseTile
        entity <- parseEntity
        return (tile, entity)
    _ <- character '\n'
    return result
