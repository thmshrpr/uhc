module Compile where

import Lang
import Parser
import Processor
import System.Environment


main :: IO ()
main = do { args <- getArgs
          ; putStr (show args)
          ; impls <- compile "Equation"
          ; putStr $ show impls
          }

compile :: String -> IO ()
compile name = do rawlayer <- parseLayer name
                  target   <- processLayer name rawlayer 
                  let layers = hierarchy target
                  impls  <- resolveImpls layers
                  let merged = merge (reverse impls)
                  return ()

