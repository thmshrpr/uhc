% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main import(System, GetOpt, IO)
%%]

%%[8 import(UU.Parsing, UU.Pretty, EHCommon, EHScanner, GRIParser, GrinCode)
%%]

%%[8 import (FPath,GRINCCommon, CmmCodePretty)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.main
main :: IO ()
main
  =  putStrLn "grinc: not available for this version (of ehc). Code generation is added in version 8."
%%]

%%[8.main -1.main
main :: IO ()
main
  =  do  {  args <- getArgs
         ;  let  oo@(o,n,errs)  = getOpt Permute cmdLineOpts args
                 opts           = foldr ($) defaultOpts o
         ;  if optHelp opts
            then  putStrLn (usageInfo "Usage: grinc [options] [file]\n\noptions:" cmdLineOpts)
            else  if null errs
                  then  doCompileRun (if null n then "" else head n) opts
                  else  putStr (head errs)
         }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compiler driver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
parseGrin :: FPath -> Opts -> IO GrModule
parseGrin fp opts
  = do {  let fNm    = fpathToStr fp
       ;  (fn,fh) <-  if fpathIsEmpty fp
                      then  return ("<stdin>",stdin)
                      else  do  {  h <- openFile fNm ReadMode
                                ;  return (fNm,h)
                                }
       ;  tokens <- scanHandle scanOpts fn fh
       ;  gr <- parseIO (pModule) tokens
       ;  return gr
       }
%%]

%%[8
doCompileRun :: String -> Opts -> IO ()
doCompileRun fn opts
  = do { let fp = mkTopLevelFPath "grin" fn
       ; gr <- parseGrin fp opts
       ; putStrLn "input parsed..."
       }
%%]
