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

%%[8 import(UU.Parsing, UU.Pretty(pp), EHCommon, EHScanner, GRIParser, GrinCode)
%%]

%%[8 import (FPath,GRINCCommon, GRIN2Cmm, CmmCodePretty)
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

%%[8.utils
openFPath :: FPath -> IOMode -> IO (String, Handle)
openFPath fp mode | fpathIsEmpty fp = case mode of 
                                        ReadMode      -> return ("<stdin>" ,stdin )
                                        WriteMode     -> return ("<stdout>",stdout)
                                        AppendMode    -> return ("<stdout>",stdout)
                                        ReadWriteMode -> error "cannot use stdin/stdout with random access"
                  | otherwise       = do
                                        let fNm = fpathToStr fp
                                        h <- openFile fNm mode
                                        return (fNm,h)
%%]

%%[8
parseGrin :: FPath -> Opts -> IO GrModule
parseGrin fp opts
  = do { (fn,fh) <- openFPath fp ReadMode
       ; tokens  <- scanHandle scanOpts fn fh
       ; gr      <- parseIO (pModule) tokens
       ; return gr
       }
%%]

%%[8
writeCmm :: CmmUnit -> FPath -> Opts -> IO ()
writeCmm cmm fp opts
  = do {  (fn, fh) <- openFPath fp WriteMode
       ;  hPutStrLn fh (show.pp $ cmm)
       ;  hClose fh
       }
%%]

%%[8
doCompileRun :: String -> Opts -> IO ()
doCompileRun fn opts
  = do { let input = mkTopLevelFPath "grin" fn
       ; gr <- parseGrin input opts
       ; putStrLn (show gr)
       ; let cmm    = grin2cmm gr
             output = fpathSetSuff "cmm" input
       ; writeCmm cmm output opts
       }
%%]
