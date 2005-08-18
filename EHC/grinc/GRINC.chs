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
    entry <- gets csEntry
    code <- return $ dropUnusedBindings True entry code
    modify (csUpdateGrinCode code)
%%]

%%[8.dropUnusedTags import(Trf.DropUnusedTags)
caDropUnusedTags :: CompileAction ()
caDropUnusedTags = do
    putMsg VerboseALot "Remove unused tags" Nothing
    code <- gets csGrinCode
    code <- return $ dropUnusedTags code
    modify (csUpdateGrinCode code)
%%]

%%[8.addLazyApply import(Trf.BuildAppBindings)
caAddLazyApplySupport :: CompileAction ()
caAddLazyApplySupport = do
    putMsg VerboseALot "Renaming lazy apply tags" Nothing
    code   <- gets csGrinCode
    unique <- gets csUnique
    (unique, code) <- return $ buildAppBindings unique code
    modify (csUpdateGrinCode code)
    modify (csUpdateUnique unique)
%%]


%%[8.numberIdentifiers import(Trf.NumberIdents, Data.Array.IArray)
caNumberIdents :: CompileAction ()
caNumberIdents = task VerboseALot "Numbering identifiers"
    ( do { code   <- gets csGrinCode
         ; unique <- gets csUnique
         ; entry <- gets csEntry
         ; (unique, entry, code, varMap, cafMap) <- return $ numberIdents unique entry code
         ; modify (\s -> s { csMbOrigNms = Just varMap
                           , csMbCafMap = Just cafMap
                           , csMbCode = Just code
                           , csEntry = entry
                           , csUnique = unique
                           }
                  )
         ; let (low, high) = bounds $ fst varMap
         ; return (high - low)
         }
    ) (\i -> Just $ show i ++ " identifiers")
%%]

%%[8.nameIdents import(Trf.NameIdents, Data.Maybe)
caNameIdents :: CompileAction ()
caNameIdents = do
    putMsg VerboseALot "Naming identifiers" Nothing
    code  <- gets csGrinCode
    vm    <- gets csOrigNms
    cafMap <- gets csCafMap
    (cafMap, code)  <- return $ nameIdents vm cafMap code
    modify (\s -> s { csEntry     = fst vm ! getNr (csEntry s)
                    , csMbCafMap  = Just cafMap
                    , csMbCode    = Just code
                    }
           )
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
caHeapPointsTo bounds = task VerboseALot "Heap-points-to analysis" 
    ( do { code    <- gets csGrinCode
         ; cm      <- gets csCafMap
         ; (c,e,h) <- liftIO $ heapPointsToAnalysis bounds cm code
         ; modify (\s -> s { csMbHptMap = Just ((e,h), Map.empty) })
         ; return c
         }
     ) (\i -> Just $ show i ++ " iteration(s)")
           
%%]

%%[8.inline import(Trf.GrInline)
caInlineEA :: CompileAction Int
caInlineEA = do
    putMsg VerboseALot "Inlining Eval and Apply calls" Nothing
    code   <- gets csGrinCode
    hptMap <- gets csHptMap
    unique <- gets csUnique
    varMap <- gets csOrigNms
    (hptMap, unique', renMap, code)   <- return $ inlineEA hptMap unique code
    modify (\s -> s { csMbOrigNms  = Just $ mergeRenameMap varMap renMap
                    , csUnique     = unique'
                    , csMbHptMap   = Just hptMap
                    , csMbCode     = Just code
                    }
           )
    return $ unique' - unique
%%]

%%[8.sparseCase import(Trf.SparseCase)
caSparseCase :: CompileAction ()
caSparseCase = do
    putMsg VerboseALot "Removing impossible case alternatives" Nothing
    code <- gets csGrinCode
    hptMap <- gets csHptMap
    code <- return $ sparseCase hptMap code
    modify (csUpdateGrinCode code)
%%]

%%[8.eliminateCase import(Trf.CaseElimination)
caEliminateCases :: CompileAction ()
caEliminateCases = do
    putMsg VerboseALot "Removing evaluated and trivial cases" Nothing
    code <- gets csGrinCode
    code <- return $ eliminateCases code
    modify (csUpdateGrinCode code)
%%]

%%[8.propagate import(Trf.CopyPropagation)
caCopyPropagation1 :: CompileAction Bool
caCopyPropagation1 = do
    code <- gets csGrinCode
    (changed, code) <- return $ propagate code
    let msg = if changed then "Changes" else "No change"
    debugging <- gets (optDebug . csOpts)
    when debugging (liftIO $ putStrLn msg)
    modify (csUpdateGrinCode code)
    return changed

caCopyPropagation :: CompileAction ()
caCopyPropagation = task VerboseALot "Copy propagation" (caFix caCopyPropagation1) (\i -> Just $ show i ++ " iteration(s)")
%%]

%%[8.lowering import(Trf.LowerGrin)
caLowerGrin :: CompileAction ()
caLowerGrin = do
    putMsg VerboseALot "Lowering GRIN" Nothing
    code   <- gets csGrinCode
    hptMap <- gets csHptMap
    unique <- gets csUnique
    varMap <- gets csOrigNms
    (hptMap, uniq, renMap, code) <- return $ lowerGrin hptMap unique code
    modify (\s -> s { csMbOrigNms  = Just $ mergeRenameMap varMap renMap
                    , csUnique     = unique
                    , csMbHptMap   = Just hptMap
                    , csMbCode     = Just code
                    }
           )
%%]

%%[8.writeCmm import(Cmm.FromGrin, Cmm.CmmCodePretty)
caGrin2Cmm :: CompileAction CmmUnit
caGrin2Cmm = do
    code <- gets csGrinCode
    entry <- gets csEntry
    cafMap <- gets csCafMap
    return (grin2cmm entry cafMap code)

caWriteCmm :: CompileAction ()
caWriteCmm = do
    input <- gets csPath
    let output = fpathSetSuff "cmm" input
    options <- gets csOpts
    putMsg VerboseALot ("Writing " ++ fpathToStr output) Nothing
    cmm <- caGrin2Cmm
    liftIO $ writePP pp cmm output options
%%]

    -- fpathToStr
    -- fpathBase

%%[8.writeGrin import(GrinCodePretty)
caWriteGrin :: String -> CompileAction ()
caWriteGrin fn = do
    input <- gets csPath
    let output =  fpathSetBase (if null fn then fpathBase input ++ "-out" else fn) input 
    putMsg VerboseALot ("Writing " ++ fpathToStr output) Nothing
    code <- gets csGrinCode
    options <- gets csOpts
    liftIO $ writePP (ppGrModule Nothing) code output options
%%]

%%[8 import("qualified Data.Map as Map", HeapPointsToFixpoint)
doCompileRun :: String -> Opts -> IO ()
doCompileRun fn opts = let input     = mkTopLevelFPath "grin" fn
                           initState = CompileState
                               { csUnique     = 3                 -- 0,1,2 are reserved (resp: __, eval, apply)
                               , csMbCode     = Nothing
                               , csEntry      = HNm "main"
                               , csMbOrigNms  = Nothing
                               , csMbCafMap   = Nothing
                               , csMbHptMap   = Nothing
                               , csPath       = input
                               , csOpts       = opts
                               , csMsgInfo    = initMsgInfo
                               }
                           putErrs (CompileError e) = putStrLn e >> return ()
                       in drive initState putErrs (caLoad >> caAnalyse >> caNormalize >> caOptimize >> caOutput)

caLoad = task_ VerboseNormal "Loading" 
    ( do { caParseGrin
         ; caDropUnusedBindings
         ; caNumberIdents
         ; caAddLazyApplySupport
         ; debugging <- gets (optDebug . csOpts)
         ; when debugging (caWriteGrin "debug.loaded")
         }
    )

caAnalyse = task_ VerboseNormal "Analysing"
    ( do { caNormForHPT
         ; caRightSkew
         ; high <- gets csUnique
         ; caHeapPointsTo (3,high-1)

         ; debugging <- gets (optDebug . csOpts)
         ; when debugging (do { ((env, heap),_) <- gets csHptMap
                              ; vm    <- gets csOrigNms
                              ; let newVar i = (i, findNewVar vm (HNPos i))
                              ; liftIO $ do { putStrLn "*** Equations ***"
                                            ; printArray "env:"  newVar aeMod env
                                            ; printArray "heap:" id ahMod heap
                                            ; putStrLn "*** Abstract Values ***"
                                            ; printArray "env:"  newVar aeBaseSet env
                                            ; printArray "heap:" id ahBaseSet heap
                                            }
                              ; caWriteGrin "debug.analyzed"
                              }
                          )
         }
    )
    
caNormalize = task_ VerboseNormal "Normalizing" 
    ( do { caInlineEA
         ; caRightSkew
         ; caLowerGrin
         ; debugging <- gets (optDebug . csOpts)
         ; when debugging (caWriteGrin "debug.normalized")
         }
    )     
caOptimize = task_ VerboseNormal "Optimizing"
    ( do { debugging <- gets (optDebug . csOpts)
         ; caSparseCase
         ; when debugging (caWriteGrin "debug.optimized1")
         ; caEliminateCases
         ; when debugging (caWriteGrin "debug.optimized2")
         ; caCopyPropagation
         ; when debugging (caWriteGrin "debug.optimized3")
         ; when debugging (caWriteGrin "debug.optimized")
         }
    )
    
caOutput = task_ VerboseNormal "Writing code"
    ( do { caNameIdents
         ; outputGrin <- gets (optWriteGrin . csOpts)
         ; maybe (return ()) caWriteGrin outputGrin
         ; caWriteCmm
         }
    )

printArray s f g a = do
    { putStrLn s 
    ; mapM_ (\(k, v) -> putStrLn ("  " ++ show (f k) ++ " = " ++ show (g v))) (assocs a)
    }
%%]

% vim:ts=4:et:ai:
