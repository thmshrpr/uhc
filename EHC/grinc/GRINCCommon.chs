% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module GRINCCommon import(System.Console.GetOpt,EHCommon, Data.FiniteMap) export(Opts(..), defaultOpts, cmdLineOpts)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data Opts  = Options  {  optHelp           ::  Bool
                      ,  optDebug          ::  Bool
                      ,  optWriteGrin      ::  Maybe String
                      ,  optSearchPath     ::  [String]
                      ,  optVerbosity      ::  Verbosity
                      }
%%]

%%[8
defaultOpts  = Options  {  optHelp           =   False
                        ,  optDebug          =   False
                        ,  optWriteGrin      =   Nothing
                        ,  optSearchPath     =   []
                        ,  optVerbosity      =   VerboseQuiet
                        }
%%]

%%[8
cmdLineOpts  
  =  [  Option "d"  ["debug"]         (NoArg oDebug)
          "include debug info, for now: print extra progress/debug info"
     ,  Option "h"  ["help"]          (NoArg oHelp)
          "output this help"
     ,  Option "g"  ["write-grin"]       (OptArg oGrin "BASENAME")
          "Write grin code instead of C-- code after transformation"
     ,  Option "v"  ["verbose"]       (OptArg oVerbose "0|1|2")
          "be verbose, 0=quiet 1=normal 2=noisy, default=1"
     ]
  where  oHelp           o =  o { optHelp          = True    }
         oDebug          o =  o { optDebug         = True    }
         oVerbose    ms  o =  case ms of
                                Just "0"    -> o { optVerbosity     = VerboseQuiet       }
                                Just "1"    -> o { optVerbosity     = VerboseNormal      }
                                Just "2"    -> o { optVerbosity     = VerboseALot        }
                                Nothing     -> o { optVerbosity     = VerboseNormal      }
                                _           -> o
         oGrin       ms  o = o { optWriteGrin = maybe (Just "") (const ms) ms }
%%]

%%[8 export(wildcardNm, wildcardNr, evalNm, evalNr,  applyNm, applyNr, isSpecialBind, CafMap)
wildcardNm = HNm "__"
wildcardNr = HNPos 0

evalNm  =  HNm "eval"
evalNr  =  HNPos 1
applyNm =  HNm "apply"
applyNr =  HNPos 2

isSpecialBind f = f == evalNm || f == applyNm

type CafMap = FiniteMap HsName HsName
%%]

%%[8.analysis import(HeapPointsToFixpoint, Data.Array)
type Analysis        = (Array Int AbstractEnvElement, Array Int AbstractHeapElement)
getEnvVar :: Analysis -> Int -> AbstractValue
getEnvVar (ea, _) i  = aeBaseSet (ea ! i)
getHeapLoc :: Analysis -> Int -> AbstractValue
getHeapLoc (_, ha) i = ahBaseSet (ha ! i)
%%]

% vim:ts=4:et:ai:
