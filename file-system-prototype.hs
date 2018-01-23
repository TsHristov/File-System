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

main = do
  let fileSystemRoot = Tree "rootDirectory" None
  createDirectoryIfMissing False (root fileSystemRoot)
  getInput fileSystemRoot

--------------------- Impure functions - IO operations ---------------------

listDirectoryContents :: String -> IO [()]
listDirectoryContents directory = do
  dirContents <- getDirectoryContents "."
  forM (sortBy compare dirContents) (\a -> do putStrLn a)

getInput :: Tree String -> IO (Tree String)
getInput tree = do
    print tree
    putStrLn "$ "
    command <- getLine
    let argv = words command
    if (head argv) == "mkdir"
    then getInput (createSubDir tree (head $ tail argv))
    else getInput tree
