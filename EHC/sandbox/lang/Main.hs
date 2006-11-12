module Main where

import Lang
import Parser
import Processor
import System.Environment


main :: IO ()
main = do { args <- getArgs
          ; res <- if null args
                   then return errOut
                   else do let fName = head args
                           compile fName
                           return $ succesMsg fName
          ; putStr res
          }

errOut :: String
errOut = unlines [ ""
                 , "Usage:  compile <layer_name>\n"
                 , "   where <layer_name> is the name of the layer to compile"
                 , ""
                 , "Example:  compile Equational"
                 , "   Equational.inf and Equational.impl must be in the local"
                 , "   directory as should all of their parent layers."
                 , ""
                 ]

succesMsg :: String -> String
succesMsg nm = unlines [ ""
                       , "Succesfully wrote the transformed output to:  " ++ fnm
                       , ""
                       ]
   where fnm = nm ++ ".hs"

compile :: String -> IO ()
compile name = do rawlayer <- parseLayer name
                  target   <- processLayer name rawlayer 
                  let layers = hierarchy target
                  impls  <- resolveImpls layers
                  let merged = merge (reverse impls)
                  writeFile (name ++ ".hs") $ show (sem_Implementation merged name)


