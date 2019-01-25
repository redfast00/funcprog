module Types where

import qualified Data.Map.Strict as Map

import           Data.Maybe      (fromMaybe)

type GenericCoordinate a = (a, a)

type Coordinate = GenericCoordinate Int

type Tilemap = (Map.Map Coordinate Tile)

data Board = Board { tilemap     :: Tilemap
                   , sausages    :: [Sausage]
                   , player      ::  Player
                   , numsausages :: Int
                   }

instance  Show Board where
    show = showBoard

isSolved :: Board -> Bool
isSolved (Board _ sausages _ numsausages) = length sausages == numsausages && baked sausages
    where baked s = and $ fmap bakedSausage s
          bakedSausage(Sausage h h') = bakedHalf h && bakedHalf h'
          bakedHalf (Half Cooked Cooked _ _) = True
          bakedHalf _                        = False

showBoard :: Board -> String
showBoard board@(Board tilemap sausages player _)
    | isSolved board = "Opgelost!\n"
    | otherwise = unlines $ characterMap width height allPlayerChars tilemap
        where allPlayerChars = playerchars player ++ concatMap sausagechars sausages
              height = maximum (fmap fst (Map.keys tilemap))
              width  = maximum (fmap snd (Map.keys tilemap))

characterMap :: Int -> Int -> [(Coordinate, Char)] -> Tilemap -> [String]
characterMap width height lut tilemap = [concat [getSingleTileDisplay (y,x) tilemap lut
                                  | x <- [0..width] ]
                                  | y <- [0..height]]


getSingleTileDisplay :: Coordinate -> Tilemap -> [(Coordinate, Char)] -> String
getSingleTileDisplay coord tilemap lut = [underground, entity]
    where underground = case Map.lookup coord tilemap of
            Nothing    -> '?'
            Just Grass -> '+'
            Just Water -> '~'
            Just Grill -> '#'
          entity = fromMaybe ' ' (lookup coord lut )

-- |Get the characters to show for the player
playerchars :: Player -> [(Coordinate, Char)]
playerchars (Player loc dir) = [(loc, dirchar dir), (loc >+ directionToCoordinate dir, 'x')]
    where dirchar North = 'N'
          dirchar East  = 'E'
          dirchar West  = 'W'
          dirchar South = 'S'

-- |Get the characters to show for a sausage
sausagechars :: Sausage -> [(Coordinate, Char)]
sausagechars (Sausage h h') = [halfchars h, halfchars h']
    where halfchars (Half top _ dir loc) = (loc, halfchar top dir)

-- |Get the character to display for sausage half tops.
halfchar :: CookedState -> Direction -> Char
halfchar Raw North    = '∩'
halfchar Raw East     = '⊃'
halfchar Raw South    = '∪'
halfchar Raw West     = '⊂'
halfchar Cooked North = '∧'
halfchar Cooked East  = '>'
halfchar Cooked South = '∨'
halfchar Cooked West  = '<'
halfchar Burned North = '⊓'
halfchar Burned East  = '⊐'
halfchar Burned South = '⊔'
halfchar Burned West  = '⊏'

data Tile = Grill | Grass | Water deriving (Show, Eq)

cook :: CookedState -> CookedState
cook Raw    = Cooked
cook Cooked = Burned
cook Burned = Burned

data Direction = North | South | East | West deriving (Show, Eq)

data Orientation = Horizontal | Vertical deriving (Show, Eq)

directionToOrientation :: Direction -> Orientation
directionToOrientation dir
    | 0 == snd (directionToCoordinate dir) = Vertical
    | otherwise                            = Horizontal


data CookedState = Raw | Cooked | Burned
    deriving (Show, Eq)

data SausageHalf = Half { top       :: CookedState
                        , bottom    :: CookedState
                        , direction :: Direction
                        , location  :: Coordinate
                        } deriving (Show, Eq)

data Sausage = Sausage SausageHalf SausageHalf deriving (Eq)

sausageOrientation :: Sausage -> Orientation
sausageOrientation (Sausage Half{direction=dir} _) = directionToOrientation dir

data Player = Player { playerlocation  :: Coordinate
                     , playerdirection :: Direction
                     }

data Entity = PlayerEntity Direction | SausageEntity Direction

directionToCoordinate :: Direction -> Coordinate
directionToCoordinate North = (-1, 0)
directionToCoordinate East  = (0, 1)
directionToCoordinate South = (1, 0)
directionToCoordinate West  = (0, -1)

oppositeDirection :: Direction -> Direction
oppositeDirection North = South
oppositeDirection East  = West
oppositeDirection South = North
oppositeDirection West  = East

leftDirection :: Direction -> Direction
leftDirection North = West
leftDirection East  = North
leftDirection South = East
leftDirection West  = South


rightDirection :: Direction -> Direction
rightDirection North = East
rightDirection East  = South
rightDirection South = West
rightDirection West  = North

-- | Vector addition
(>+) :: Num a => GenericCoordinate a -> GenericCoordinate a -> GenericCoordinate a
(x, y) >+ (x', y') = (x+x', y+y')

-- | Vector subtraction
(>-) :: Num a => GenericCoordinate a -> GenericCoordinate a -> GenericCoordinate a
(x, y) >- (x', y') = (x-x', y-y')

-- | Vector scaling
(>*) :: Num a => GenericCoordinate a -> a -> GenericCoordinate a
(x, y) >* n = (x * n, y * n)

-- |Given a player direction and move direction
-- | returns the coordinates of the movement and the new direction
inputDirection :: Direction -> Direction -> (Coordinate, Direction)
inputDirection pdir mdir
    | mdir == pdir                   = (directionToCoordinate mdir, pdir)
    | mdir == oppositeDirection pdir = (directionToCoordinate mdir, pdir)
    | otherwise                      = ((0, 0), mdir)
