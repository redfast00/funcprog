import           System.Environment (getArgs)

import           CogsParser         (cogsParser)
import           CogsSolver         (isSolved)
import           Graphic            (game)
import           Parser             (parseStatement)
import           Types

main :: IO ()
main = do args <- getArgs
          case args of
              ["--test", filename] -> parseFile filename >>= validate printSolved
              [filename]           -> parseFile filename >>= validate game
              _                    -> putStrLn "Unrecognized option"

parseFile :: String -> IO (Either String Board)
parseFile filename = do
    content <- readFile filename
    return $ parseStatement content cogsParser

validate :: (a -> IO ()) -> Either String a -> IO ()
validate _      (Left msg)      = putStrLn $ "INVALID: " ++ msg
validate action (Right content) = action content

printSolved :: Board -> IO ()
printSolved board = putStrLn (if isSolved board then "SOLVED" else "NOT SOLVED")
