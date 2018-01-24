import System.Directory
import System.IO
import Control.Monad
import Data.List as List (sortBy)
import FileSystem

main = do
  let fileSystem = Directory "rootDirectory" None
  makeDirectory (currentDirectory fileSystem)
  getInput fileSystem
  
--------------------- Impure functions - IO operations ---------------------

listDirectoryContents :: FilePath -> IO [()]
listDirectoryContents directory = do
  dirContents <- getDirectoryContents "."
  forM (sortBy compare dirContents) (\a -> do putStrLn a)

makeDirectory :: FilePath -> IO ()
makeDirectory path =  createDirectoryIfMissing False path

getInput :: FileSystem -> IO ()
getInput fileSystem = takeInput fileSystem
  where takeInput tree = do
          print tree
          putStrLn "$ "
          command <- getLine
          let argv  = words command
              argv1 = head $ tail argv
          case (head argv) of
            -- Make directories:
            "mkdir" -> getInput (mkdir tree argv1)
            -- Print working directory:
            "pwd"   -> print $ pwd tree
            -- Change working directory:
            "cd"    -> getInput (cd tree argv1)
          getInput fileSystem
