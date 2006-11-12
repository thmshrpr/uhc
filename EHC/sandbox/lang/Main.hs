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
errOut = "The application shoud be passed at least one parameter, being the name of the implementation to be transformed. E.g. if your implementation is in 'Equational.impl', you should invoke the appliation with the parameter 'Equational'\n" 

succesMsg :: String -> String
succesMsg fName = "Succesfully wrote the transformed output to " ++ fName ++ ".hs\n"

compile :: String -> IO ()
compile name = do rawlayer <- parseLayer name
                  target   <- processLayer name rawlayer 
                  let layers = hierarchy target
                  impls  <- resolveImpls layers
                  let merged = merge (reverse impls)
                  writeFile (name ++ ".hs") $ show (sem_Implementation merged name)

