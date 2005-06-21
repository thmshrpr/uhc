% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main import(System.IO, System.Environment, System.Console.GetOpt, Control.Monad.Error, Control.Monad.State)
%%]

%%[8 import(UU.Parsing, UU.Pretty, EHCommon, EHScanner, GrinCode)
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
                  else  mapM_ (\o -> putStr $ "grinc: " ++ o) errs
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
  = do { (fn, fh) <- openFPath fp WriteMode
       ; hPutStrLn fh (show.f $ text)
       ; hClose fh
       }
%%]

%%[8.parse import(GRIParser)
parseGrin :: FPath -> Opts -> IO (String, GrModule)
parseGrin fp opts = do
    (fn,fh) <- openFPath fp ReadMode
    tokens  <- scanHandle scanOpts fn fh
    gr      <- parseIO (pModule) tokens
    return (fn, gr)

caParseGrin :: CompileAction ()
caParseGrin = do
    putMsg VerboseALot "Parsing" Nothing
    path <- gets csPath
    opts <- gets csOpts
    (fn, code) <- liftIO $ parseGrin path opts
    modify (csUpdateGrinCode code)
%%]


%%[8.dropEvalAndApply import(Trf.DropUnusedBindings)
caDropUnusedBindings :: CompileAction ()
caDropUnusedBindings = do
    putMsg VerboseALot "Remove unused function bindings" Nothing
    code <- gets csGrinCode
    code <- return $ dropUnusedBindings True code
    modify (csUpdateGrinCode code)
%%]

%%[8.numberIdentifiers import(Trf.NumberIdents, Data.Array.IArray)
caNumberIdents :: CompileAction ()
caNumberIdents = task VerboseALot "Numbering identifiers"
    ( do { code   <- gets csGrinCode
         ; unique <- gets csUnique
         ; (unique, code, varMap, cafMap) <- return $ numberIdents unique code
         ; modify (\s -> s { csMbOrigNms = Just varMap, csMbCafMap = Just cafMap, csMbCode = Just code, csUnique = unique } )
         ; let (low, high) = bounds varMap
         ; return (high - low)
         }
    ) (\i -> Just $ show i ++ " identifiers")
%%]

%%[8.nameIdents import(Trf.NameIdents)
caNameIdents :: CompileAction ()
caNameIdents = do
    putMsg VerboseALot "Naming identifiers" Nothing
    code <- gets csGrinCode
    vm   <- gets csOrigNms
    code <- return $ nameIdents vm code
    modify (csUpdateGrinCode code)
%%]

%%[8.normForHPT import(Trf.NormForHPT)
caNormForHPT :: CompileAction ()
caNormForHPT = task VerboseALot "Normalizing"
    ( do { code   <- gets csGrinCode
         ; unique <- gets csUnique
         ; (unique', code) <- return $ normForHPT unique code
         ; modify (csUpdateGrinCode code)
         ; modify (csUpdateUnique unique')
         ; return (unique' - unique)
         }
    ) (\i -> Just $ show i ++ " variable(s) introduced")
%%]

%%[8.rightSkew import(Trf.RightSkew)
caRightSkew1 :: CompileAction Bool
caRightSkew1 = do 
    code <- gets csGrinCode
    (code, changed) <- return $ rightSkew code
    modify (csUpdateGrinCode code)
    let msg = if changed then "Changes" else "No change"
    debugging <- gets (optDebug . csOpts)
    when debugging (liftIO $ putStrLn msg)
    return changed

caRightSkew :: CompileAction ()
caRightSkew = task VerboseALot "Unskewing" (caFix caRightSkew1) (\i -> Just $ show i ++ " iteration(s)")
%%]

%%[8.heapPointsTo import(GrPointsToAnalysis)
caHeapPointsTo :: (Int, Int) -> CompileAction ()
caHeapPointsTo bounds = do
    putMsg VerboseALot "Heap-points-to analysis" Nothing
    code   <- gets csGrinCode
    cm     <- gets csCafMap
    result <- liftIO $ heapPointsToAnalysis bounds cm code
    modify (\s -> s { csMbHptMap = Just (result, emptyFM) })
%%]

%%[8.inline import(Trf.GrInline)
caInlineEA :: CompileAction Int
caInlineEA = do
    putMsg VerboseALot "Inlining Eval and Apply calls" Nothing
    code   <- gets csGrinCode
    hptMap <- gets csHptMap
    unique <- gets csUnique
    (hptMap, unique', code)   <- return $ inlineEA hptMap unique code
    modify (csUpdateUnique unique')
    modify (csUpdateHptMap hptMap)
    modify (csUpdateGrinCode code)
    return $ unique' - unique
%%]

%%[8.lowering import(Trf.LowerGrin)
caLowerGrin :: CompileAction ()
caLowerGrin = do
    putMsg VerboseALot "Lowering GRIN" Nothing
    code <- gets csGrinCode
    code <- return $ lowerGrin code
    modify (csUpdateGrinCode code)
%%]

%%[8.writeCmm import(Cmm.FromGrin, Cmm.CmmCodePretty)
caGrin2Cmm :: CompileAction CmmUnit
caGrin2Cmm = do
    code <- gets csGrinCode
    return (grin2cmm code)

caWriteCmm :: CompileAction ()
caWriteCmm = do
    putMsg VerboseALot "Writing C--" Nothing
    cmm <- caGrin2Cmm
    input <- gets csPath
    let output = fpathSetSuff "cmm" input
    options <- gets csOpts
    --debugging <- gets (optDebug . csOpts)
    --when debugging (liftIO $ putStrLn "=============" >> putStrLn (show cmm))
    liftIO $ writePP pp cmm output options
%%]

%%[8.writeGrin import(GrinCodePretty)
caWriteGrin :: String -> CompileAction ()
caWriteGrin fn = do
    putMsg VerboseALot "Writing Grin" Nothing
    code <- gets csGrinCode
    input <- gets csPath
    let output =  fpathSetBase (if null fn then fpathBase input ++ "-out" else fn) input
    options <- gets csOpts
    liftIO $ writePP (ppGrModule Nothing) code output options
%%]

%%[8 import(Data.FiniteMap, HeapPointsToFixpoint)
doCompileRun :: String -> Opts -> IO ()
doCompileRun fn opts = let input     = mkTopLevelFPath "grin" fn
                           initState = CompileState
                               { csUnique     = 3                 -- 0,1,2 are reserved (resp: __, eval, apply)
                               , csMbCode     = Nothing
                               , csMbOrigNms  = Nothing
                               , csMbCafMap   = Nothing
                               , csMbHptMap   = Nothing
                               , csPath       = input
                               , csOpts       = opts
                               , csMsgInfo    = initMsgInfo
                               }
                           putErrs (CompileError e) = putStrLn e >> return ()
                       in drive initState putErrs (caLoad >> caAnalyse >> caNormalize >> caOutput)

caLoad = task_ VerboseNormal "Loading" 
    ( do { caParseGrin
         ; caDropUnusedBindings
         ; caNumberIdents
         }
    )

caAnalyse = task_ VerboseNormal "Analysing"
    ( do { caNormForHPT
         ; caRightSkew
         ; high <- gets csUnique
         ; caHeapPointsTo (3,high-1)

         ; debugging <- gets (optDebug . csOpts)
         ; when debugging (do { ((env, heap),_) <- gets csHptMap
                              ; liftIO $ do { putStrLn "*** Equations ***"
                                            ; printArray "env:"  aeMod env
                                            ; liftIO $ printArray "heap:" ahMod heap
                                            ; putStrLn "*** Abstract Values ***"
                                            ; printArray "env:"  aeBaseSet env
                                            ; printArray "heap:" ahBaseSet heap
                                            }
                              }
                          )
         }
    )
    


caNormalize = task_ VerboseNormal "Normalizing" 
    ( do { caInlineEA
         ; debugging <- gets (optDebug . csOpts)
         ; when debugging (caWriteGrin "")
         ; caLowerGrin
         }
    )
    
caOutput = task_ VerboseNormal "Writing code"
    ( do { caNameIdents
         ; outputGrin <- gets (optWriteGrin . csOpts)
         ; maybe (return ()) caWriteGrin outputGrin
         ; caWriteCmm
         }
    )

printArray s f a = do
    { putStrLn s 
    ; mapM_ (\(k, v) -> putStrLn ("  " ++ show k ++ " = " ++ show (f v))) (assocs a)
    }
%%]

% vim:ts=4:et:ai:
