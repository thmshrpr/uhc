{- ----------------------------------------------------------------------------------------
   what    : Working with directories
   expected: ok
---------------------------------------------------------------------------------------- -}

module Main where

import System.Directory
import Data.Maybe

root :: String
root = "filesForIOTesting"

pFile :: String
pFile = root ++ "/" ++ "empty"

main :: IO ()
main = do
  cd   <- getCurrentDirectory
  rcd  <- makeRelativeToCurrentDirectory cd
  putStrLn rcd
  setCurrentDirectory root
  cd'  <- getCurrentDirectory
  setCurrentDirectory cd
  rcd' <- makeRelativeToCurrentDirectory cd'
  putStrLn $ show (root == rcd')

  croot <- canonicalizePath root
  crcd' <- canonicalizePath cd'
  putStrLn $ show (croot == crcd')
  --putStrLn croot
  
  files  <- getDirectoryContents root
  putStrLn $ show files
  

  perm   <- getPermissions pFile
  putStrLn $ show perm
  setPermissions pFile perm{executable=True}
  perm'  <- getPermissions pFile
  putStrLn $ show perm'
  setPermissions pFile perm'{executable=False}
  perm'' <- getPermissions pFile
  putStrLn $ show (perm == perm'')

  modTime <- getModificationTime pFile -- [@@@] check this
  putStrLn $ show modTime

  ex <- doesFileExist pFile
  putStrLn $ show ex
 
  exec <- findExecutable "ghc"
  putStrLn $ show (isJust exec)

  hd   <- getHomeDirectory             -- [@@@] comment this
  audd <- getAppUserDataDirectory "cabal"
  udd  <- getUserDocumentsDirectory
  td   <- getTemporaryDirectory
  putStrLn hd
  putStrLn audd
  putStrLn udd
  putStrLn td

