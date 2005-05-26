% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%[8 import(Control.Monad.Error,Control.Monad.State, Data.Maybe)
%%]

%%[8.State import(GRINCCommon, EHCommon, GrinCode, FPath)
data CompileState = CompileState
	{ csUnique :: Int
	, csName   :: HsName
	, csMbCode :: Maybe GrModule 
	, csPath   :: FPath
	, csOpts   :: Opts
	}

emptyState :: CompileState
emptyState = CompileState
	{ csUnique = 0
	, csName   = HNm "<UNKNOWN>"
	, csMbCode = Nothing
	, csPath   = emptyFPath
	, csOpts   = defaultOpts
	}

csGrinCode           = fromJust . csMbCode
csIsParsed           = isJust   . csMbCode
csUpdateGrinCode c s = s { csMbCode = Just c }
csUpdateUnique   u s = s { csUnique = u }
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

%%[8.utils
putMsg :: Verbosity -> String -> (Maybe String) -> CompileAction ()
putMsg minVerbosity msg mbMsg =  harden_ $ do
	currentVerbosity <- gets (optVerbosity . csOpts)
	guard (currentVerbosity >= minVerbosity)
	modName <- gets csName
	path    <- gets csPath
        let msg2    = maybe "" (\m -> ", " ++ m) mbMsg
	let message =            strBlankPad 25 msg
	              ++ " "  ++ strBlankPad 15 (show modName)
	              ++ " (" ++ fpathToStr path ++ msg2 ++ ")"
	liftIO $ putStrLn message
%%]

%%[8.fixpoint
caFix :: CompileAction Bool -> CompileAction ()
caFix step = do
	changes <- step 
	when changes (caFix step)
%%]

% vim:ts=4:et:ai:
