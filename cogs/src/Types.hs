{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types where

type Coordinate = (Int, Int)

instance Num a => Num (a, a) where
      negate (x, y)      = (-x, -y)
      (x, y) + (x', y') = (x+x', y+y')
      (*)         = undefined
      fromInteger = undefined
      abs         = undefined
      signum      = undefined

type Direction = Coordinate

north, east, south, west :: Direction
north = (-1, 0)
east  = (0, 1)
south = (1, 0)
west  = (0, -1)

data Source = Source { getSourcePosition :: Coordinate
                     , getSourceDirection :: Direction
                     } deriving (Eq, Show)

data Sink   = Sink { getSinkPosition :: Coordinate
                   , getSinkDirection :: Direction
                   } deriving (Eq, Show)

data Tile = Tile { isMoveable :: Bool
                 , getConnectedDirections :: [Direction]
                 , getTilePosition :: Coordinate
                 } deriving (Eq, Show)

data Board = Board { getWidth   :: Int
                   , getHeight  :: Int
                   , getSources :: [Source]
                   , getSinks   :: [Sink]
                   , getTiles   :: [Tile]
                   } deriving (Show)

oppositeDirection :: Direction -> Direction
oppositeDirection = negate

allDirections :: [Direction]
allDirections = [north, east, south, west]

inBoard :: Board -> Coordinate -> Bool
inBoard board = inRange (getWidth board) (getHeight board)

inRange :: Int -> Int -> Coordinate -> Bool
inRange width height (x, y) = 0 <= x && x < height && 0 <= y && y < width
