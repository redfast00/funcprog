module CogsSolver (isSolved, isInflated, moveTiles) where

import           Types

import           Control.Applicative ((<$>))
import           Control.Monad       ((>=>))
import           Data.List           ((\\))
import           Data.Maybe          (listToMaybe, mapMaybe)
import           Data.Ratio          ((%))

moveTiles :: Coordinate -> Board -> Board
moveTiles coord board
    | not $ inBoard board coord = board
    | otherwise = case findHole board coord of
        Nothing    -> board
        Just (changedTiles, direction) -> board{getTiles=newTiles}
            where newTiles = (getTiles board \\ changedTiles) ++ moveTilesBy direction changedTiles

-- | Finds the direction a list of coordinates will be moved in given a coordinate
recoverDirection :: Coordinate -> [Coordinate] -> Coordinate
recoverDirection coord movedCoords = head $ filter ((==1) . abs . uncurry (+)) $ fmap (\x -> x - coord) movedCoords

moveTilesBy :: Coordinate -> [Tile] -> [Tile]
moveTilesBy coord = fmap (\t -> t{getTilePosition=getTilePosition t + coord})

-- | Finds a hole above, below, on the right or on the left of a coordinate. Hole also has to be moveable
findHole :: Board -> Coordinate -> Maybe ([Tile], Coordinate)
findHole board from@(x, y) = listToMaybe $ fmap (\a -> (changedTiles a,recoverDirection from $ between from a)) $ filter allGood $ heighmatching ++ widthmatching
    where heighmatching = [(x', y) | x' <- [0..getHeight board - 1] \\ (fst <$> filter ((==y) . snd) tilecoordinates)]
          widthmatching = [(x, y') | y' <- [0..getWidth board  - 1] \\ (snd <$> filter ((==x) . fst) tilecoordinates)]
          tilecoordinates = fmap getTilePosition changeableTiles
          changeableTiles = filter ((\(cx, cy) -> cx == x || cy == y) . getTilePosition) (getTiles board)
          allGood coordinate = all isMoveable $ changedTiles coordinate
          changedTiles coordinate = filter ((`elem` between from coordinate ) . getTilePosition) changeableTiles

-- | Gets all coordinates between two coordinates
between :: Coordinate -> Coordinate -> [Coordinate]
between (cx, cy) (ex, ey)
    | cx == ex = [(cx, y) | y <- [min cy ey..max cy ey]]
    | cy == ey = [(x, cy) | x <- [min cx ex..max cx ex]]
    | otherwise = error "no between" -- It would be cool to do this in a pure way

-- | Checks if a board is solved (all sinks inflated)
isSolved :: Board -> Bool
isSolved board@Board{getSinks=sinks} = all (isInflated' connectedList) sinks
    where connectedList = getConnectedList board

-- | Get a list of lists of sinks filled up by sources (each list corresponds to a source)
getConnectedList :: Board -> [[Sink]]
getConnectedList board@Board{getSources=sources} = mapMaybe (connected >=> startPropagate) sources
    where connected (Source pos dir) = findConnectedPipes board [dir] pos
          startPropagate = propagate board []

-- Two isInflated functions, so that isInflated can be exported
-- and we retain the efficiency of isSolved: we don't have to calculate the propagation each time

-- | Checks if a sink is inflated
isInflated :: Board -> Sink -> Bool
isInflated board = isInflated' (getConnectedList board)

-- | Checks if a sink is inflated
isInflated' :: [[Sink]] -> Sink -> Bool
isInflated' connectedLists sink = 1 <= sum (map (\x -> 1 % length x) $ filter (sink `elem`) connectedLists)

-- | Finds a tile with a coordinate
findTile :: Board -> Coordinate -> Maybe Tile
findTile board pos =  listToMaybe $ filter ((pos==) . getTilePosition) (getTiles board)

-- | Check if source/sink seals a coordinate not on the board
isEdgeStopper :: Coordinate -> Board -> Bool
isEdgeStopper coord board
    | inBoard board coord = False
    | not $ any ((==coord) . getSinkPosition) (getSinks board) = True
    | not $ any ((==coord) . getSourcePosition) (getSources board) = True
    | otherwise = False

-- | Check if steam will leak out of a tile on a size
isLeaky :: Coordinate -> Board -> Direction -> Bool
isLeaky coord board dir = case otherTileResult of
    Nothing -> not $ isEdgeStopper connectedTileCoord board
    (Just othertile) -> oppositeDirection dir `notElem` getConnectedDirections othertile
    where connectedTileCoord = coord + dir
          otherTileResult = findTile board connectedTileCoord

-- | Finds a list of pipes connecting to a coordinate, keeping in mind that pipes can leak
findConnectedPipes :: Board -> [Direction] -> Coordinate -> Maybe [Tile]
findConnectedPipes board connections pos
    | or $ fmap (isLeaky pos board) connections = Nothing
    | otherwise                                 = traverse (findTile board) connectedCoordinates
    where connectedCoordinates = filter (inBoard board) $ fmap (+ pos) connections

-- | Finds the sinks connected to a tile
findSinks :: Board -> Tile -> [Sink]
findSinks board (Tile _ connections pos) = concatMap sinksConnected connectedEdges
    where connectedEdges = filter (not . inBoard board) $ fmap (+ pos) connections
          sinksConnected coord = filter ((==coord) . getSinkPosition) (getSinks board)

-- | Propagates the steam from a source, keeping in mind that pipes can leak
propagate :: Board -> [Tile] -> [Tile] -> Maybe [Sink]
propagate _ _ [] = Just []
propagate board seen (tile:todo) = case connected of
    Nothing -> Nothing
    (Just othertiles) -> fmap (++foundSinks) (propagate board (newSeen ++ seen) (newTodo ++ todo))
        where newSeen = filter (`notElem` seen) othertiles
              newTodo = filter (`notElem` todo) newSeen
              foundSinks = findSinks board tile
    where connected = findConnectedPipes board (getConnectedDirections tile) (getTilePosition tile)
