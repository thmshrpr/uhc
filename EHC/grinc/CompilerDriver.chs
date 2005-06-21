% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%[8 import(Control.Monad.Error,Control.Monad.State, Data.Maybe)
%%]

%%[8.State import(GRINCCommon, EHCommon, GrinCode, FPath)
data CompileState = CompileState
	{ csUnique    :: Int
	, csMbCode    :: Maybe GrModule 
    , csMbOrigNms :: Maybe IdentNameMap
    , csMbCafMap  :: Maybe CafMap
    , csMbHptMap  :: Maybe HptMap
	, csPath      :: FPath
	, csOpts      :: Opts
    , csMsgInfo   :: (Int, Bool)
	}

csGrinCode           = fromJust . csMbCode
csOrigNms            = fromJust . csMbOrigNms
csCafMap             = fromJust . csMbCafMap
csHptMap             = fromJust . csMbHptMap
csIsParsed           = isJust   . csMbCode
csUpdateGrinCode c s = s { csMbCode = Just c }
csUpdateUnique   u s = s { csUnique = u }
csUpdateHptMap   m s = s { csMbHptMap = Just m }
%%]

%%[8.Errors
newtype CompileError = CompileError String
	deriving (Show)

instance Error CompileError where
	noMsg    = CompileError "internal error"
	strMsg s = CompileError s
%%]

%%[8.CompilerDriver
type CompileAction a = ErrorT CompileError (StateT CompileState IO) a

drive :: CompileState -> (CompileError -> IO a) -> CompileAction a -> IO a
drive initState errorHandler action = do
	result <- doAction action
	case result of
		Right suc -> return suc
		Left  err -> errorHandler err
	where
	doAction = flip evalStateT initState . runErrorT

%%]

%%[8.errorHandling
ignoreErrors :: (Monad m) => a -> b -> m a
-- ignoreErrors = const . return -- does not typecheck in HM (but does in ML-F)
ignoreErrors v e = return v

harden   :: (MonadError e m) => a -> m a -> m a
harden v =  flip catchError (ignoreErrors v)

ignoreErrors_ :: (Monad m) => b -> m ()
ignoreErrors_ = ignoreErrors ()

harden_  :: (MonadError e m) => m() -> m ()
harden_  =  harden ()
%%]

%%[8.messages
initMsgInfo :: (Int, Bool) -- indent, FirstMessageInLevel
initMsgInfo = (0, False)

putLn = putStrLn ""

putMsg :: Verbosity -> String -> (Maybe String) -> CompileAction ()
putMsg minVerbosity msg mbMsg =  harden_ $ do
    currentVerbosity <- gets (optVerbosity . csOpts)
    guard (currentVerbosity >= minVerbosity)
    (indent, first) <- gets csMsgInfo
    when first (liftIO putLn)
    let msg2    = maybe "" (\m -> " (" ++ m ++ ")") mbMsg
        message = replicate indent ' ' ++ strBlankPad 36 msg ++ msg2
    liftIO $ putStrLn message
    when first (modify (\s -> s { csMsgInfo = (indent, False) }))
    

task_ :: Verbosity -> String -> CompileAction a -> CompileAction ()
task_ minVerbosity td ca = task minVerbosity td ca (const Nothing)

task :: Verbosity -> String -> CompileAction a -> (a -> Maybe String) -> CompileAction ()
task minVerbosity taskDesc ca f = do 
    { startMsg minVerbosity taskDesc
    ; r <- ca
    ; let message = f r
    ; finishMsg minVerbosity message
    }
    where
    startMsg :: Verbosity -> String -> CompileAction ()
    startMsg minVerbosity msg =  harden_ $ do
        currentVerbosity <- gets (optVerbosity . csOpts)
        guard (currentVerbosity >= minVerbosity)
        (indent, first) <- gets csMsgInfo
        when first (liftIO putLn)
        let message = replicate indent ' ' ++ strBlankPad 36 msg
        liftIO $ putStr message
        modify (\s -> s { csMsgInfo = (indent+4, True) })
    
    finishMsg :: Verbosity -> Maybe String -> CompileAction ()
    finishMsg minVerbosity mbMsg =  harden_ $ do
        currentVerbosity <- gets (optVerbosity . csOpts)
        guard (currentVerbosity >= minVerbosity)
        (oldIndent, first) <- gets csMsgInfo
        let indent = oldIndent - 4
            outputMsg m = putStrLn $ if first then " (" ++ m ++ ")" else replicate indent ' ' ++ m
        liftIO $ maybe (if first then putLn else return ()) outputMsg mbMsg
        modify (\s -> s { csMsgInfo = (indent, False) })
%%]

%%[8.fixpoint
caFix :: CompileAction Bool -> CompileAction Int
caFix step = caFixCount 1
    where
    caFixCount n = do
        changes <- step 
        if changes then (caFixCount $ n+1) else return n
%%]

% vim:ts=4:et:ai:
