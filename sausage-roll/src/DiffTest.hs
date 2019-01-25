
import Data.List          (intersect)
import Data.Maybe         (mapMaybe)
import Control.Monad      (liftM)
import System.Environment (getArgs)
import System.Exit        (ExitCode(..))

import System.Directory   (listDirectory)
import System.FilePath    (addExtension, stripExtension, (</>))
import System.Process     (readProcessWithExitCode)

main :: IO ()
main = (findTests <$> (head <$> getArgs >>= listUnrelativeDir)) >>= mapM_ runTest

-- | Gegeven een mapnaam, lijst alle bestanden op die zich in deze map
-- bevinden. De mapnaam wordt voorgevoegd aan de opgelijste bestanden.
listUnrelativeDir :: FilePath -> IO [FilePath]
listUnrelativeDir d = map (d </>) <$> listDirectory d

-- | Gegeven een mapnaam en de bestanden die zich in deze map bevinden, geeft
-- `findTests` de gevonden testen terug.
findTests :: [FilePath] -> [(String, FilePath, FilePath, FilePath)]
findTests listing = map replaceExts $ withExt "in" `intersect` withExt "steps" `intersect` withExt "out"
  where
    withExt ext = mapMaybe (stripExtension ext) listing
    replaceExts name = ( name
                       , addExtension name "in"
                       , addExtension name "steps"
                       , addExtension name "out"
                       )

-- | Voert een test uit en schrijft het resultaat naar stdout.
runTest :: (String, FilePath, FilePath, FilePath) -> IO ()
runTest (name, infile, stepsfile, outfile) = do
    putStr $ "running test " ++ name ++ "... "
    steps  <- lines <$> readFile stepsfile
    output <- readFile outfile
    result <- readProcessWithExitCode "stack" ["build", "--exec", unwords ("sausage-roll" : infile : steps)] ""
    case result of
        (ExitFailure i, _,      _) -> putStrLn $ "failed; non-zero exit code (" ++ show i ++ ") :-("
        (ExitSuccess,   stdout, _) -> putStrLn $ if stdout == output then "correct!" else "wrong!" ++ "\nexpected-->\n" ++ output ++ "<--\nGOT:\n" ++ stdout ++ "<--\n"
