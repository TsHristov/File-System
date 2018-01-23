import System.Directory
import System.IO
import Control.Monad
import Data.List as List (sortBy)

data Tree a = Tree { root :: a , subtrees :: TreeList a } deriving (Show)
data TreeList a = None | SubTree { firstTree :: Tree a,
                                   restTrees :: TreeList a
                                 } deriving (Show)

createSubDir :: Tree a -> a -> Tree a
createSubDir (Tree root None) directory = Tree root (SubTree (Tree directory None) None)

-- Print working directory:
pwd :: Tree FilePath -> FilePath
pwd directory = root directory

-- Change working directory:
cd :: Tree FilePath -> FilePath -> Tree FilePath
cd directoryTree path
  | path == "."  = directoryTree
  | otherwise    = firstTree $ subtrees directoryTree -- Stub

main = do
  let fileSystemRoot = Tree "rootDirectory" None
  createDirectoryIfMissing False (root fileSystemRoot)
  getInput fileSystemRoot

--------------------- Impure functions - IO operations ---------------------

listDirectoryContents :: String -> IO [()]
listDirectoryContents directory = do
  dirContents <- getDirectoryContents "."
  forM (sortBy compare dirContents) (\a -> do putStrLn a)

getInput :: Tree FilePath -> IO ()
getInput directoryTree = takeInput directoryTree
  where takeInput tree = do
          print tree
          putStrLn "$ "
          command <- getLine
          let argv  = words command
              argv1 = head $ tail argv
          case (head argv) of
            -- Make directory:
            "mkdir" -> getInput (createSubDir tree argv1)
            -- Print working directory:
            "pwd"   -> print $ pwd tree
            -- Change working directory:
            "cd"    -> getInput (cd tree argv1)
          getInput directoryTree
