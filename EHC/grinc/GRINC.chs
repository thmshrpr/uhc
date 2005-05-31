% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main import(System, GetOpt, IO, Control.Monad.Error, Control.Monad.State)
%%]

%%[8 import(UU.Parsing, UU.Pretty, EHCommon, EHScanner, GRIParser, GrinCode, GrinCodePretty)
%%]

%%[8 import (FPath,GRINCCommon, CompilerDriver)
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


writePP ::  (a -> PP_Doc) -> a -> FPath -> Opts -> IO ()
writePP f text fp opts
  = do {  (fn, fh) <- openFPath fp WriteMode
       ;  hPutStrLn fh (show.f $ text)
       }
%%]

%%[8.parse
parseGrin :: FPath -> Opts -> IO (String, GrModule)
parseGrin fp opts = do
	(fn,fh) <- openFPath fp ReadMode
	tokens  <- scanHandle scanOpts fn fh
	gr      <- parseIO (pModule) tokens
	return (fn, gr)

caParseGrin :: CompileAction ()
caParseGrin = do
	putMsg VerboseNormal "Parsing" Nothing
	path <- gets csPath
	opts <- gets csOpts
	(fn, code) <- liftIO $ parseGrin path opts
	-- let name = HNm fn
	let (GrModule_Mod name _ _ _ _) = code
	modify (\s -> s { csName = name })
	modify (csUpdateGrinCode code)
%%]

%%[8.numberIdentifiers import(NumberIdents, Data.Array.IArray)
caNumberIdents :: CompileAction (IdentNameMap, CafMap)
caNumberIdents = do
	putMsg VerboseNormal "Numbering identifiers" Nothing
	code   <- gets csGrinCode
	unique <- gets csUnique
        (unique, code, varMap, cafMap) <- return $ numberIdents unique code
	modify (csUpdateGrinCode code)
	modify (csUpdateUnique unique)
	let (low, high) = bounds varMap
	putMsg VerboseALot "Identifiers numbered" (Just (show (high-low) ++ " identifiers"))
	return (varMap, cafMap)
%%]

%%[8.nameIdents import(NameIdents)
caNameIdents :: IdentNameMap -> CompileAction ()
caNameIdents m = do
	putMsg VerboseNormal "Numbering identifiers" Nothing
	code <- gets csGrinCode
        code <- return $ nameIdents m code
	modify (csUpdateGrinCode code)
%%]

%%[8.normForHPT import(NormForHPT)
caNormForHPT :: CompileAction ()
caNormForHPT = do
	putMsg VerboseNormal "Normalizing" Nothing
	code   <- gets csGrinCode
	unique <- gets csUnique
        (unique', code) <- return $ normForHPT unique code
	modify (csUpdateGrinCode code)
	modify (csUpdateUnique unique')
	putMsg VerboseALot "Normalized" (Just (show (unique'-unique) ++ " variable(s) introduced"))
%%]

%%[8.rightSkew import(RightSkew)
caRightSkew1 :: CompileAction Bool
caRightSkew1 = do 
	code <- gets csGrinCode
        (code, changed) <- return $ rightSkew code
	modify (csUpdateGrinCode code)	
	let msg2 = if changed then "Changes" else "No change"
	putMsg VerboseALot "Right skewed" (Just msg2)
	return changed

caRightSkew :: CompileAction Int
caRightSkew = caFix caRightSkew1
%%]

%%[8.heapPointsTo import(GrPointsToAnalysis)
caHeapPointsTo :: CafMap -> CompileAction ()
caHeapPointsTo cm = do
	putMsg VerboseNormal "Heap-points-to analysis" Nothing
	code <- gets csGrinCode
        code <- return $ addPointsToInfo cm code
	modify (csUpdateGrinCode code)	
%%]

%%[8.lowering import(LowerGrin)
caLowerGrin :: CompileAction ()
caLowerGrin = do
	putMsg VerboseNormal "Lowering GRIN" Nothing
	code <- gets csGrinCode
        code <- return $ lowerGrin code
	modify (csUpdateGrinCode code)	
%%]

%%[8.writeCmm import(GRIN2Cmm, CmmCodePretty)
caGrin2Cmm :: CompileAction CmmUnit
caGrin2Cmm = do
	code <- gets csGrinCode
	return (grin2cmm code)

caWriteCmm :: CompileAction ()
caWriteCmm = do
	cmm <- caGrin2Cmm
	input <- gets csPath
        let output = fpathSetSuff "cmm" input
	options <- gets csOpts
	when (optDebug options) (liftIO $ putStrLn "=============" >> putStrLn (show cmm))
	liftIO $ writePP pp cmm output options
%%]

%%[8.writeGrin
caWriteGrin :: String -> CompileAction ()
caWriteGrin fn = do
	code <- gets csGrinCode
	input <- gets csPath
        let output =  fpathSetBase (if null fn then fpathBase input ++ "-out" else fn) input
	options <- gets csOpts
	liftIO $ writePP (ppGrModule Nothing) code output options
%%]

%%[8 import(Data.FiniteMap)
doCompileRun :: String -> Opts -> IO ()
doCompileRun fn opts = let input                    = mkTopLevelFPath "grin" fn
                           initState = CompileState
                               { csUnique = 3                 -- 0,1,2 are reserved (resp: __, eval, apply)
                               , csName   = HNm fn
                               , csMbCode = Nothing
                               , csPath   = input
                               , csOpts   = opts
                               }
                           putErrs (CompileError e) = putStrLn e >> return ()
                       in drive initState putErrs compileActions

compileActions :: CompileAction ()
compileActions = do
	caParseGrin
	(vm, cm) <- caNumberIdents
	caNormForHPT
	n <- caRightSkew
	putMsg VerboseALot "unskewed" (Just $ show n ++ " iteration(s)")
	caHeapPointsTo cm

	caNameIdents vm
	outputGrin <- gets (optWriteGrin . csOpts)
	maybe (throwError $ strMsg "No C-- output for the moment") caWriteGrin outputGrin
	throwError (strMsg $ "compilation stopped. cm=" ++ show (fmToList cm))

	caLowerGrin
	caWriteCmm
%%]
