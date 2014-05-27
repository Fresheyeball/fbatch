module Main (main) where

import Data.List(isInfixOf)
import Data.String.Utils(replace)
import System.Environment(getArgs)
import System.FilePath((</>))
import System.Console.ANSI
import System.Directory(renameFile, renameDirectory, getDirectoryContents, doesDirectoryExist)

getDeltas :: FilePath -> String -> [String] -> [(String, String)]
getDeltas r r' ps = let o = filter (r `isInfixOf`) ps
                        d = map (replace r r') o
                        in zip o d

rename :: FilePath -> FilePath -> IO()
rename x y = do   
  isDir <- doesDirectoryExist x
  if isDir
  then renameDirectory x y else renameFile x y

putStrInColor :: String -> Color -> IO()
putStrInColor x y = do
  setSGR [ SetConsoleIntensity NormalIntensity 
         , SetColor Foreground Vivid y]
  putStr x
  setSGR []

renameDeltaInBase :: FilePath -> (FilePath, FilePath) -> IO ()
renameDeltaInBase b (o,d) = do
  putStr "renaming: "
  (o ++ "\t") `putStrInColor` Blue
  putStr " -> "
  (d ++ "\n") `putStrInColor` Blue
  rename (b </> o) (b </> d)

main :: IO()
main = do
  [reject, replacement, directory] <- getArgs
  files <- getDirectoryContents directory
  let deltas = getDeltas reject replacement files
  mapM_ (renameDeltaInBase directory) deltas
  putStrLn $ (show . length $ deltas) ++ " <- files renamed"