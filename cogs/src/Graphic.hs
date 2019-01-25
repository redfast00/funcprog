module Graphic (game) where

import           CogsSolver                         (isInflated, moveTiles)
import           Types

import           Graphics.Gloss                     (Picture (..), play,
                                                     rectangleSolid)
import           Graphics.Gloss.Data.Color          (Color, makeColorI)
import           Graphics.Gloss.Interface.Pure.Game (Display (..), Event (..),
                                                     Key (..), KeyState (..),
                                                     MouseButton (..))

type GameCoordinate = (Float, Float)

scale, breedte, hoogte :: Int
scale = 50
breedte = 20
hoogte = 20

translateCoordinates :: Coordinate -> GameCoordinate
translateCoordinates (x, y) = (fromIntegral $ scale * y, negate $ fromIntegral $ scale * x)

reverseTranslate :: GameCoordinate -> Coordinate
reverseTranslate (x, y) = (negate $ round $ y / fromIntegral scale, round $ x / fromIntegral scale)

vakje :: Picture
vakje = rectangleSolid (fromIntegral scale) (fromIntegral scale)

window :: Display
window = InWindow "Cog block"
                (breedte * scale, scale * hoogte)
                (0,0)

game :: Board -> IO ()
game startBoard = play window   backgroundColor
               60            startBoard
               displayBoard  handleInput
               step

-- | Step function that returns the same board unaltered
step :: Float -> Board -> Board
step = flip const

displayBoard ::  Board -> Picture
displayBoard board = Pictures $ fmap displayTile (getTiles board) ++ fmap displaySource (getSources board) ++ fmap (displaySink board) (getSinks board)

relativePipeSize :: Float
relativePipeSize = 3

movableColor, immovableColor, backgroundColor, pipeColor, balloonColor, sourceColor :: Color
movableColor = makeColorI 136 35 14 255
immovableColor = makeColorI 110 110 110 255
backgroundColor = makeColorI 40 44 52 255
pipeColor = makeColorI 20 20 20 255
balloonColor = makeColorI 255 0 0 255
sourceColor = makeColorI 0 255 0 255

pipepart :: Picture
pipepart = Color pipeColor $ rectangleSolid (fromIntegral scale / relativePipeSize) (fromIntegral scale / relativePipeSize)

displayTile :: Tile -> Picture
displayTile tile = Translate x y $ Pictures $ tileback : fmap displayPipe (getConnectedDirections tile)
    where (x, y) = translateCoordinates $ getTilePosition tile
          tileback = Color (if isMoveable tile then movableColor else immovableColor) vakje
          displayPipe direction = Pictures [outer, inner]
             where inner = pipepart
                   (px, py) = translateCoordinates direction
                   outer = Translate (px / relativePipeSize) (py / relativePipeSize) pipepart

displaySource :: Source -> Picture
displaySource source = Translate x y $ Pictures [tileback, connector, circle, innercircle]
    where (x, y) = translateCoordinates $ getSourcePosition source
          tileback = Color immovableColor vakje
          (px, py) = translateCoordinates $ getSourceDirection source
          connector = Translate (px / relativePipeSize) (py / relativePipeSize) pipepart
          size = 1 / 4
          circle = Color pipeColor $ ThickCircle (fromIntegral scale * size) (fromIntegral scale * size)
          innercircle = Color sourceColor $ ThickCircle 0 (fromIntegral scale * size)

displaySink :: Board -> Sink -> Picture
displaySink board sink = Translate x y $ Pictures [tileback, connector, balloon]
    where (x, y) = translateCoordinates $ getSinkPosition sink
          tileback = Color immovableColor vakje
          (px, py) = translateCoordinates $ getSinkDirection sink
          connector = Translate (px / relativePipeSize) (py / relativePipeSize) pipepart
          inflated = isInflated board sink
          deflatedSize = 1 / 3
          inflatedSize = deflatedSize * 2
          balloon = Color balloonColor $ ThickCircle 0 (fromIntegral scale * (if inflated then inflatedSize else deflatedSize))

handleInput :: Event -> Board -> Board
handleInput (EventKey (MouseButton LeftButton) Down _ loc) = moveTiles coords
    where coords = reverseTranslate loc
handleInput _ = id
