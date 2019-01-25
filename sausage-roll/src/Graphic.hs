module Graphic where

import           Push
import           Types

import           Control.Applicative                ((<$>))

import qualified Data.Map.Strict                    as Map
import           Graphics.Gloss                     (Picture (..), play,
                                                     rectangleSolid, translate)
import           Graphics.Gloss.Data.Color          (black, blue, green,
                                                     makeColorI, orange)
import           Graphics.Gloss.Interface.Pure.Game (Display (..), Event (..),
                                                     Key (..), KeyState (..),
                                                     SpecialKey (..))

type GameCoordinate = (Int, Int)

vakje :: Picture
vakje  = rectangleSolid (fromIntegral scale) (fromIntegral scale)

coordNaarFiguur :: GameCoordinate -> Picture -> Picture
coordNaarFiguur (y, x) = translate nx ny
                      where nx = (fromIntegral x - fromIntegral breedte / 2 + 1) * fromIntegral scale
                            ny = (fromIntegral hoogte / 2 - fromIntegral y - 1)  * fromIntegral scale

scale, breedte, hoogte :: Int
scale = 40
breedte = 20
hoogte = 20

window :: Display
window = InWindow "Sausage roll, just roll with it"
                   (breedte * scale, scale * hoogte)
                   (0,0)

game :: Board -> IO ()
game startBoard = play window   blue
                  60            startBoard
                  displayBoard  handleInput
                  step

-- | Step function that returns the same board unaltered
step :: Float -> Board -> Board
step = flip const

displayBoard ::  Board -> Picture
displayBoard board@(Board tilemap sausages player _)
    | isSolved board = Blank
    | otherwise = Pictures $ displayTilemap tilemap:displayPlayer player:fmap displaySausage sausages

displayTilemap :: Tilemap -> Picture
displayTilemap tilemap = Pictures $ displayTile <$> Map.assocs tilemap

displayTile :: (Coordinate, Tile) -> Picture
displayTile (coord, Grass) = Color green  $ coordNaarFiguur coord vakje
displayTile (coord, Grill) = Color orange $ coordNaarFiguur coord vakje
displayTile (coord, Water) = Color blue   $ coordNaarFiguur coord vakje

sausageSize, playerSize, forkSize :: Float
sausageSize = 0.6
playerSize  = 0.8
forkSize    = 0.2

displayPlayer :: Player -> Picture
displayPlayer Player{playerlocation=loc, playerdirection=dir} = Pictures [playerpic, forkpic]
    where playerpic = Color black $ coordNaarFiguur loc $ Scale playerSize playerSize vakje
          forkpic   = Color black $ coordNaarFiguur (loc >+ directionToCoordinate dir) $ Scale forkSize forkSize vakje

displaySausage :: Sausage -> Picture
displaySausage (Sausage h h') = Pictures [displaySausageHalf h, displaySausageHalf h']

displaySausageHalf :: SausageHalf -> Picture
-- TODO add translate for direction so sausages overlap
displaySausageHalf Half{top=top, location=loc, direction=dir} = Color (getCookedColor top) $ Translate dx dy $ coordNaarFiguur loc $ Scale sausageSize sausageSize vakje
    where getCookedColor Raw    = makeColorI 255 127 127 255 -- pink
          getCookedColor Cooked = makeColorI 105 61  61  255 -- brown
          getCookedColor Burned = black
          (ndx, ndy) = directionToCoordinate dir
          (dy, dx)   = (fromIntegral ndx, fromIntegral (- ndy)) >* ((1 - sausageSize) / 2) >* fromIntegral scale

isKey :: SpecialKey -> Event -> Bool
isKey k1 (EventKey (SpecialKey k2)    Down  _ _ ) = k1 == k2
isKey _  _                                        = False


handleInput :: Event -> Board -> Board
handleInput event
    | isKey KeyUp event                              = executeDirection North
    | isKey KeyDown event                            = executeDirection South
    | isKey KeyLeft event                            = executeDirection West
    | isKey KeyRight event                           = executeDirection East
    | otherwise                                      = id
