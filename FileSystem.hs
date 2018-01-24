module FileSystem where

data Directory a = Directory { currentDirectory :: a ,
                               subDirectories   :: SubDirectories a
                             } 
  
data SubDirectories a = None | SubDirectory { firstSubDirectory  :: Directory a,
                                              restSubDirectories :: SubDirectories a
                                            } 

type FileSystem = Directory FilePath

instance (Show a) => Show (Directory a) where
  show a = (show $ currentDirectory a) ++ (show $ subDirectories a)

instance (Show a) => Show (SubDirectories a) where
  show None = ""
  show a    = "\n |-" ++ (show $ firstSubDirectory a) ++ show (restSubDirectories a)

-- Make directories:
mkdir :: FileSystem -> FilePath -> FileSystem
mkdir (Directory root None) directory = Directory root (SubDirectory (Directory ('/':root ++ '/':directory) None) None)

-- Print working directory:
pwd :: FileSystem -> FilePath
pwd = currentDirectory

-- Change working directory:
cd :: FileSystem -> FilePath -> FileSystem
cd fileSystem@(Directory rootDirectory subDirectories) path
  | path == rootDirectory || path == "." = fileSystem
