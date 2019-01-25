module Push where

import           Prelude         hiding (lookup)

import           Data.List       (delete)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)

import           Types


actionDirection :: Direction -> Direction -> ActionDirection
actionDirection pdir mdir
    | mdir == pdir                   = forward
    | mdir == oppositeDirection pdir = backward
    | mdir == leftDirection pdir     = turnleft
    | otherwise                      = turnright

executeDirection :: Direction -> Board -> Board
executeDirection mdir oldBoard = case Map.lookup (playerlocation newPlayer) (tilemap oldBoard) of
    Just Grass -> oldBoard {player = newPlayer, sausages = newSausages}
    _          -> oldBoard -- We can't move on non-grass blocks or outside of the map
    where pdir = (playerdirection . player) oldBoard
          (offset, newdir) = inputDirection pdir mdir
          newPlayer = Player ((playerlocation . player) oldBoard >+ offset) newdir
          actionToExecute = actionDirection pdir mdir
          newSausages = executeAction actionToExecute oldBoard

type ActionDirection = Player -> [Collision]

executeAction :: ActionDirection -> Board -> [Sausage]
executeAction actionGenerator board = resolveCollision (tilemap board) (actionGenerator (player board)) (sausages board)

-- |Generates all collisions if player goes forward
forward :: ActionDirection
forward (Player playercoord playerdirection) = [Collision (playercoord >+ (directionToCoordinate playerdirection >* 2)) playerdirection]

-- |Generates all collisions if player goes backward
backward :: ActionDirection
backward (Player playercoord playerdirection) = [Collision (playercoord >- directionToCoordinate playerdirection) opposite]
    where opposite = oppositeDirection playerdirection

-- |Generates all collisions if player turns left
turnleft :: ActionDirection
turnleft (Player playercoord playerdirection) = [collision, collision']
    where offset = (directionToCoordinate . leftDirection) playerdirection >+ directionToCoordinate playerdirection
          collision  = Collision (playercoord >+ offset) (leftDirection playerdirection)
          offset' = (directionToCoordinate . leftDirection) playerdirection
          collision' = Collision (playercoord >+ offset') (oppositeDirection playerdirection)

-- |Generates all collisions if player turns right
turnright :: ActionDirection
turnright (Player playercoord playerdirection) = [collision, collision']
    where offset = (directionToCoordinate . rightDirection) playerdirection >+ directionToCoordinate playerdirection
          collision  = Collision (playercoord >+ offset) (rightDirection playerdirection)
          offset' = (directionToCoordinate . rightDirection) playerdirection
          collision' = Collision (playercoord >+ offset') (oppositeDirection playerdirection)

-- |Flips a sausage upside down
rotateSausage :: Sausage -> Sausage
rotateSausage (Sausage half half') = Sausage (rotate half) (rotate half')
    where rotate original = original {top = bottom original, bottom = top original}

-- |Move a sausage in a direction, either by rolling or pushing it
move :: Sausage -> Direction -> Sausage
move sausage@(Sausage h h') dir
    | directionToOrientation dir == sausageOrientation sausage = pushed
    | otherwise                                                = rolled
    where pushed = Sausage (h {location = location h >+ offset}) (h' {location = location h' >+ offset})
          rolled = rotateSausage pushed
          offset = directionToCoordinate dir

-- | Does the appropriate thing to a sausage half
sausageLeft :: Tile -> SausageHalf -> SausageHalf
sausageLeft Grill half = half {bottom = cook (bottom half)}
sausageLeft _     half = half

-- | Moves a sausage on the map (or off the map) and cooks it if needed
moveSausage :: Tilemap -> Sausage -> Direction -> Maybe Sausage
moveSausage tilemap sausage movedirection = case (newtile, newtile') of
    (Water, Water) -> Nothing -- If the sausage moves in the water, it disappears
    _              -> Just $ Sausage (sausageLeft newtile hnew) (sausageLeft newtile' hnew')
    where (Sausage hnew hnew') = move sausage movedirection
          newtile  = fromMaybe Water (Map.lookup (location hnew) tilemap)
          newtile' = fromMaybe Water (Map.lookup (location hnew') tilemap)


data Collision = Collision Coordinate Direction

-- |Converts a sausage into a list of collisions given a direction in which it's pushed
sausageToCollisions :: Direction -> Sausage -> [Collision]
sausageToCollisions dir (Sausage h h') = [Collision (location h) dir, Collision (location h') dir]

-- |Searches for a sausage that is on the coordinate
searchSausage :: [Sausage] -> Coordinate -> Maybe Sausage
searchSausage [] _ = Nothing
searchSausage (sausage@(Sausage half half'):rest) coordinate = if firstoverlap || secondoverlap
                                                               then Just sausage
                                                               else searchSausage rest coordinate
    where firstoverlap  = coordinate == location half
          secondoverlap = coordinate == location half'

resolveCollision :: Tilemap -> [Collision] -> [Sausage] -> [Sausage]
resolveCollision _       []                     sausages = sausages
resolveCollision tilemap (Collision cc cd:rest) sausages = case searchSausage sausages cc of
    Nothing -> resolveCollision tilemap rest sausages -- There is no sausage in the way of this collision
    (Just sausage) -> case movedSausageResult of
            Nothing -> resolveCollision tilemap rest removedSausages -- Sausage disappeared
            Just movedSausage -> movedSausage:resolveCollision tilemap (sausageToCollisions cd movedSausage ++ rest) removedSausages
            where removedSausages = delete sausage sausages -- Sausages without sausage that moved
                  movedSausageResult = moveSausage tilemap sausage cd
