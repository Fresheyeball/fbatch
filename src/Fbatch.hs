module Fbatch where

import Data.List(isInfixOf)
import Data.String.Utils(replace)
import System.Environment(getArgs)
import System.FilePath((</>))
import System.Console.ANSI
import System.Directory(renameFile, renameDirectory, getDirectoryContents, doesDirectoryExist, doesFileExist)
import qualified Control.Monad.Parallel as P

type FileName  = String
type FileDelta = (FileName, FileName)

data Rename = RenameDirectory FileName FileName
            | RenameFile      FileName FileName
            | RenameNothing
            deriving (Show, Eq)

data Printable = In String Color
               | Normally String

replaceItoken :: String -> String -> (FileName, Int) -> String
replaceItoken r r' (o, i) = let t = replace "#{i}" (show i) r'
                            in replace r t o 

getDeltas :: String -> String -> [FileName] -> [FileDelta]
getDeltas r r' ps = let o = filter (r `isInfixOf`) ps
                        d = map (replaceItoken r r') (zip o [0..])
                    in zip o d

getRename :: FilePath -> FilePath -> IO Rename
getRename x y = do
  d <- doesDirectoryExist x
  f <- doesFileExist x
  if d 
    then return $ RenameDirectory x y
    else if f 
    then return $ RenameFile      x y
    else return   RenameNothing

rename :: Rename -> IO()
rename (RenameDirectory x y) = renameDirectory x y
rename (RenameFile      x y) = renameFile      x y
rename  RenameNothing        = return ()

print' :: Printable -> IO String
print' (s `In` c) = do
  setSGR [SetColor Foreground Vivid c]
  putStr s
  setSGR []
  return s
print' (Normally s) = do 
  putStr s
  return s

printRename :: (FileName, FileName) -> IO String
printRename (o, d) = do
  w <- print' $ Normally "renaming: "
  x <- print' $ (o ++ "\t") `In` Yellow
  y <- print' $ Normally " -> "
  z <- print' $ (d ++ "\n") `In` Blue
  return $ w ++ x ++ y ++ z

renameFromCli :: IO()
renameFromCli = do
  [reject, replacement, dir] <- getArgs
  files                      <- getDirectoryContents dir

  let deltas = getDeltas reject replacement files

  _ <- P.mapM (\(o, d) -> rename =<< getRename (dir </> o) (dir </> d)) deltas
  mapM_ printRename deltas
  putStrLn $ (show . length $ deltas) ++ " <- files renamed"