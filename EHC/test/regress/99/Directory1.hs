{- ----------------------------------------------------------------------------------------
   what    : Create and remove directories
   expected: ok
---------------------------------------------------------------------------------------- -}

module Main where

import System.Directory

root :: String
root = "filesForIOTesting"

cdir :: String
cdir = root  ++ "/" ++ "CreateDir"

rootn :: String
rootn = root ++ "/" ++ "CreateDirIfMissing" 

cdim :: String
cdim = rootn ++ "/" ++ "NewDir"

main :: IO ()
main = do
  createDirectory cdir
  cde   <- doesDirectoryExist cdir
  putStrLn $ show cde
  createDirectoryIfMissing True cdim
  cdime <- doesDirectoryExist cdim
  putStrLn $ show cdime
  removeDirectory cdir
  removeDirectory cdim
  removeDirectory rootn

