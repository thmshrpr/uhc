%%[1 module {%{EH}AnnInfo} 
%%]

      %%[8 export(PhiInfo(..),PhiAnn(..),meet,join,getPhi,isStrictPhi)
%%]

%%[8 import({%{EH}Base.Common})
%%]

%%[8
{-| PhiInfo is the information about the annotations 
-}

data PhiInfo = Strict
             | Lazy
             | PhiVar HsName
             | Meet PhiInfo PhiInfo
             | Join PhiInfo PhiInfo
             | PhiEmpty
             deriving Show
{-|
-}

data PhiTau = PhiTArrow PhiTau PhiInfo PhiInfo
            | PhiTAnn   PhiInfo

data PhiSig = PhiSigTau    PhiTau
            | PhiSigForAll HsName PhiSig

data PhiAnn = PhiAnnArrow PhiAnn PhiInfo PhiInfo
            | PhiAnn      PhiInfo
            | NoPhiAnn 
            deriving Show


meet :: PhiInfo -> PhiInfo -> PhiInfo
Lazy   `meet` Lazy    = Lazy
Lazy   `meet` Strict  = Strict
Lazy   `meet` r       = r
Strict `meet` _       = Strict
r      `meet` Lazy    = r
r      `meet` Strict  = Strict
r1     `meet` r2      = Meet r1 r2

join :: PhiInfo -> PhiInfo -> PhiInfo
Lazy   `join` _       = Lazy
Strict `join` Lazy    = Lazy
Strict `join` Strict  = Strict
Strict `join` r       = r
r      `join` Lazy    = Lazy
r      `join` Strict  = r
r1     `join` r2      = Join r1 r2

getPhi :: PhiAnn -> PhiInfo
getPhi NoPhiAnn              = PhiEmpty  -- This could be source of errors
getPhi (PhiAnnArrow _ phi _) = phi
getPhi (PhiAnn      phi)     = phi

isStrictPhi :: PhiInfo -> Bool
isStrictPhi Strict = True
isStrictPhi _      = False
%%]
