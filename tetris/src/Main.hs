
-- standard library imports
import           Control.Applicative                ((<$>))
import           Data.List                          (intersect)
import           Data.Maybe                         (listToMaybe)

-- third party imports
import           Graphics.Gloss                     (Picture (..), play,
                                                     rectangleSolid, translate)
import           Graphics.Gloss.Data.Color          (Color, makeColorI)
import           Graphics.Gloss.Interface.Pure.Game (Display (..), Event (..),
                                                     Key (..), KeyState (..),
                                                     SpecialKey (..))

-- local application imports

type Coordinate = (Int, Int)

-- | Een enkele blok met een (x,y) positie.
newtype Block = Block (Int, Int) deriving (Eq, Show)

{-|

  We stellen een vallende tetromino voor door een lijst van blokken.

  - `size` is de breedte van de tetromino, a.k.a. het vierkantje waarbinnen
    hij draait. Zie onderstaand voorbeeld.
  - `position` is de huidige positie van de tetromino, de linkerbovenhoek van
    het vierkantje gedefinieerd door size.
  - `blocks` zijn de 4 blokken van deze tetromino, met positie relatief ten
    opzicht van de positie van de tetromino.

       0 1 2 3 4 x         0 1 2 3 4 x    Wie zien hier hoe de i-tetromino
    o-------------->    o-------------->  gedraaid wordt naar rechts. De
    |                   |                 min-karakters teken je niet, maar
   0|  - - - -         0|  - - x -        zij geven het vierkant bepaald
   1|  x x x x         1|  - - x -        door `size` aan. De x-karakters
   2|  - - - -         2|  - - x -        stellen de blokken voor. We zien
   3|  - - - -         3|  - - x -        hoe het vierkant (size) en de
   4|                  4|                 positie onder de rotatie niet
  y |                 y |                 veranderen.
    v                   v

       0 1 2 3 4 x         0 1 2 3 4 x    Wie zien hier hoe de s-tetromino
    o-------------->    o-------------->  gedraaid wordt naar rechts.
    |                   |                 Opnieuw gaan `size` en `position`
   0|  - x x           0|  - x -          niet veranderen, enkel de
   1|  x x -           1|  - x x          relatieve positie van de blokjes.
   2|  - - -           2|  - - x
   3|                  3|
  y |                 y |
    v                   v

-}
data Tetromino = Tetromino
    { size     :: Int         -- ^ omvattend vierkant
    , position :: Coordinate  -- ^ positie linkerbovenhoek vierkant
    , blocks   :: [Block]     -- ^ blokken relatief t.o.v. position
    } deriving (Eq, Show)

-- | Het bord met de vaste (niet-vallende) vierkantjes.
newtype Board = Board [Block] deriving (Eq, Show)

-- | Tetris bestaat uit vaste en vallende blokjes.
data Tetris = Tetris
    { tetrisBoard     :: Board
    , tetrisTetromino :: Tetromino
    } deriving (Eq, Show)

schaal :: Int
schaal = 30

breedte :: Int
breedte = 10

hoogte :: Int
hoogte = 30

speed :: Float
speed = 0.10

data Wereld = Wereld
    { tetris :: Tetris
    , time   :: Float
    , alive  :: Bool
    }

-- | Beschikbare tetronimo's
tetrominoes :: [Tetromino]
tetrominoes = [i,o,t,j,l,s,z]
    where i = Tetromino 4 (0,0) [ Block (0,1), Block (1,1), Block (2,1), Block (3,1) ]
          o = Tetromino 2 (0,0) [ Block (0,0), Block (1,0), Block (0,1), Block (1,1) ]
          t = Tetromino 3 (0,0) [ Block (1,0), Block (0,1), Block (1,1), Block (2,1) ]
          j = Tetromino 3 (0,0) [ Block (0,0), Block (0,1), Block (1,1), Block (2,1) ]
          l = Tetromino 3 (0,0) [ Block (2,0), Block (0,1), Block (1,1), Block (2,1) ]
          s = Tetromino 3 (0,0) [ Block (1,0), Block (2,0), Block (0,1), Block (1,1) ]
          z = Tetromino 3 (0,0) [ Block (0,0), Block (0,1), Block (1,1), Block (2,1) ]

-- | Tetris!
main :: IO ()
main = play venster      backgroundColor
            60           startWereld
            maakFiguren  verwerkInvoer
            stap

stap :: Float -> Wereld -> Wereld
stap timePassed wereld
    | not $ alive wereld      = wereld
    | tooHigh (tetris wereld) = wereld {alive = False}
    | otherwise               = wereld {time = ntime, tetris = newtetris}
    where ntime = time wereld + timePassed
          previousEvents = floor (time wereld / speed)
          currentEvents = floor (ntime / speed)
          newtetris = iterate (stapTetris currentEvents) (tetris wereld) !! fromIntegral (currentEvents - previousEvents)

stapTetris :: Int -> Tetris -> Tetris
stapTetris seed oldTetris
    | doesCollide movedDown = keepDoingItUntilItsTheSame shred (solidifyTetromino seed oldTetris)
    | otherwise = movedDown
    where movedDown = oldTetris {tetrisTetromino = moveTetromino 0 1 (tetrisTetromino oldTetris)}

-- | Converts the blocks in a tetromino to blocks on the board
solidifyTetromino :: Int -> Tetris -> Tetris
solidifyTetromino seed (Tetris (Board existingBlocks) tetromino) = Tetris newBoard newTetromino
    where newBoard     = Board (existingBlocks ++ (Block <$> calculateTetrominoCoordinates tetromino))
          newTetromino = tetrominoes !! (seed `mod` length tetrominoes)

-- | Apply a function until it converges
keepDoingItUntilItsTheSame :: Eq a => (a -> a) -> a -> a
keepDoingItUntilItsTheSame f b = if next == b then b else keepDoingItUntilItsTheSame f next
    where next = f b

-- | Remove a full row if possible
shred :: Tetris -> Tetris
shred t = case findFullRow (tetrisBoard t) of
    (Just rownum) -> t {tetrisBoard = removeFullRow (tetrisBoard t) rownum}
    Nothing       -> t

-- | Searches for a full row
findFullRow :: Board -> Maybe Int
findFullRow board = listToMaybe $ filter (\y -> all (\x -> (x, y) `elem` boardCoords) [0..breedte - 1]) [0..hoogte]
    where boardCoords = calculateBoardCoordinates board

-- | Removes a row and moves rows above down
removeFullRow :: Board -> Int -> Board
removeFullRow board rownum = Board $ Block <$> fmap (\(x, y) -> if y < rownum then (x, y + 1) else (x, y)) (filter (\(_, y) -> y /= rownum) (calculateBoardCoordinates board))

-- | Converts a tetromino to its coordinates
calculateTetrominoCoordinates :: Tetromino -> [Coordinate]
calculateTetrominoCoordinates (Tetromino _ (bx, by) list) = fmap (\(Block (x, y)) -> (bx + x, by + y)) list

-- | Converts a board to the coordinates of the blocks in that board
calculateBoardCoordinates :: Board -> [Coordinate]
calculateBoardCoordinates (Board list) = fmap (\(Block coord) -> coord) list

-- | Check if a tetromino is entirely in the board
isTetrominoInBoard :: Tetromino -> Bool
isTetrominoInBoard tetromino = all inboard $ calculateTetrominoCoordinates tetromino
    where inboard (x, y) = 0 <= x && x < breedte && 0 <= hoogte && y < hoogte

-- | Check if a tetromino collides with either the borders of the board or with block in the board
doesCollide :: Tetris -> Bool
doesCollide (Tetris board t) = not $ isTetrominoInBoard t &&
    null
       (calculateBoardCoordinates board `intersect`
          calculateTetrominoCoordinates t)

startWereld :: Wereld
startWereld = Wereld (Tetris (Board []) (head tetrominoes)) 0 True

vakje :: Picture
vakje  = rectangleSolid l l
    where l = fromIntegral schaal

-- | Converts a coordinate to a gloss Picture
coordNaarFiguur :: Coordinate -> Picture
coordNaarFiguur (x, y) = translate nx ny vakje
                         where nx = fromIntegral $ (x - breedte `div` 2) * schaal + schaal `div` 2
                               ny = fromIntegral $ (hoogte `div` 2 - y)  * schaal - schaal `div` 2

-- | Converts the board to a gloss Picture
boardNaarFiguur :: Board -> Picture
boardNaarFiguur board = Color blockColor $ Pictures $ coordNaarFiguur <$> calculateBoardCoordinates board

-- | Converts a tetromino to a gloss Picture
tetrominoNaarFiguur :: Tetromino -> Picture
tetrominoNaarFiguur tetromino = Color tetrominoColor $ Pictures $ coordNaarFiguur <$> calculateTetrominoCoordinates tetromino

-- | Converts a world to a gloss Picture
maakFiguren :: Wereld -> Picture
maakFiguren (Wereld (Tetris board tetromino) _ _) = Pictures [box, tetrominoNaarFiguur tetromino, boardNaarFiguur board]

boardColor, tetrominoColor, blockColor, backgroundColor :: Color
boardColor      = makeColorI 46 53 50 255
tetrominoColor  = makeColorI 139 38 53 255
blockColor      = makeColorI 98 124 133 255
backgroundColor = makeColorI 0 0 0 255

box :: Picture
box = Color boardColor (rectangleSolid schaalb schaalh)
      where schaalb = fromIntegral (schaal * breedte)
            schaalh = fromIntegral (schaal * hoogte)

venster :: Display
venster = InWindow "Supermegacoole tetris" -- TODO make it cool
                   (breedte * schaal, schaal * hoogte)
                   (0,0)

-- | Does a move if it doesn't cause the tetromino to collide
doIfPossible :: (Tetromino -> Tetromino) -> Tetris -> Tetris
doIfPossible moveFunction oldTetris
    | doesCollide newTetris = oldTetris
    | otherwise             = newTetris
    where newTetris = oldTetris {tetrisTetromino = moveFunction (tetrisTetromino oldTetris)}

-- | Check if there are block in the board that are too high
tooHigh :: Tetris -> Bool
tooHigh (Tetris board _) = any (\(_, y) -> y <= 0) (calculateBoardCoordinates board)

-- | Moves the tetromino by (dx, dy)
moveTetromino :: Int -> Int -> Tetromino -> Tetromino
moveTetromino dx dy t@(Tetromino _ (x, y) _) = t {position = (x + dx, y + dy)}

-- | Rotates the tetromino to the left (counterclockwise)
draaiTetromino :: Tetromino -> Tetromino
draaiTetromino t = t {blocks = fmap rotateBlock (blocks t)}
    where rotateBlock (Block (x, y)) = Block (y, size t - x - 1)

-- | Handles keypress events
verwerkInvoer :: Event -> Wereld -> Wereld
verwerkInvoer (EventKey (Char 'r')            Down _ _) wereld = wereld {tetris = doIfPossible draaiTetromino (tetris wereld)}
verwerkInvoer (EventKey (SpecialKey KeyRight) Down _ _) wereld = wereld {tetris = doIfPossible (moveTetromino 1 0) (tetris wereld)}
verwerkInvoer (EventKey (SpecialKey KeyLeft)  Down _ _) wereld = wereld {tetris = doIfPossible (moveTetromino (-1) 0) (tetris wereld)}
verwerkInvoer (EventKey (SpecialKey KeyEnter) Down _ _) _      = startWereld
verwerkInvoer _ wereld = wereld
