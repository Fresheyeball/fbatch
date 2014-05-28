module Main (main) where

import Data.List(isInfixOf)
import Data.String.Utils(replace)
import System.Environment(getArgs)
import System.FilePath((</>))
import System.Console.ANSI
import System.Directory(renameFile, renameDirectory, getDirectoryContents, doesDirectoryExist)
import qualified Control.Monad.Parallel as P

type FileName = String

replaceItoken :: String -> String -> (FileName, Int) -> String
replaceItoken r r' (o, i) = let t = replace "#{i}" (show i) r'
                            in replace r t o 

getDeltas :: String -> String -> [FileName] -> [(FileName, FileName)]
getDeltas r r' ps = let o = filter (r `isInfixOf`) ps
                        d = map (replaceItoken r r') (zip o [0..])
                    in zip o d

rename :: FilePath -> FilePath -> IO()
rename x y = do
  d <- doesDirectoryExist x
  if d
  then renameDirectory x y
  else renameFile x y

putStrInColor :: String -> Color -> IO()
s `putStrInColor` c = do
  setSGR [SetColor Foreground Vivid c]
  putStr s
  setSGR []

renameDeltaInBase :: FilePath -> (FileName, FileName) -> IO()
renameDeltaInBase b (o, d) = do
  putStr "renaming: "
  (o ++ "\t") `putStrInColor` Yellow
  putStr " -> "
  (d ++ "\n") `putStrInColor` Blue
  rename (b </> o) (b </> d)

main :: IO()
main = do
  [reject, replacement, directory] <- getArgs
  files <- getDirectoryContents directory

  let deltas = getDeltas reject replacement files

  P.mapM (renameDeltaInBase directory) deltas
  putStrLn $ (show . length $ deltas) ++ " <- files renamed"