
import           System.Environment (getArgs)
import           System.IO

import           BoardParser
import           Parser
import           Push
import           Types
import           Graphic (game)


main :: IO ()
main = do (boardFile:steps) <- getArgs
          boardString       <- readFile boardFile
          hSetBuffering stdout NoBuffering
          let parsedBoard = parseBoardFromString boardString
          if null steps
            then game parsedBoard
            else putStr . unlines . map printBoard $ scanl walk parsedBoard (map (parseDir . head) steps)

-- Hieronder volgen enkele definities om duidelijk te maken hoe bovenstaande code werkt.
-- Je wil deze waarschijnlijk gewoon wegsmijten en vervangen door iets nuttig.

parseDir :: Char -> Direction
parseDir 'N' = North
parseDir 'S' = South
parseDir 'E' = East
parseDir 'W' = West
parseDir a   = error $ "Unknown direction:" ++ [a]

parseBoardFromString :: String -> Board
parseBoardFromString s = case executeParser parseBoard s of
    (Left msg)     -> error msg
    (Right result) -> result

printBoard :: Board -> String
printBoard = show

walk :: Board -> Direction -> Board
walk = flip executeDirection
