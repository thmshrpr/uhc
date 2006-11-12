module Main where

import Lang
import Parser
import Processor
import System.Environment


main :: IO ()
main = do { args <- getArgs
          ; putStr (show args)
          ; impls <- compile "Known"
          ; putStr $ impls
          }

compile :: String -> IO (String)
compile name = do rawlayer <- parseLayer name
                  target   <- processLayer name rawlayer 
                  let layers = hierarchy target
                  impls  <- resolveImpls layers
                  let merged = merge (reverse impls)
                  return $ show (sem_Implementation merged name)

