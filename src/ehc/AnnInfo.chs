%%[1 module {%{EH}AnnInfo} 
%%]

%%[8 export(Ann(..),AnnTy(..),meet,join,getAnn,isStrictAnn,mkPhiVar,anotateTy,substAnnTy,substAnnArrow)
%%]

%%[8 import({%{EH}Base.Common}, {%{EH}Base.Builtin}, {%{EH}Ty})
%%]

%%[8
{-| PhiInfo is the information about the annotations 
-}
type PhiId = UID

data  Ann    = Strict
             | Lazy
             | AnnVar PhiId
             | Meet Ann Ann
             | Join Ann Ann
             | PhiEmpty -- ??
             deriving (Show,Eq)

type AnnTyVarId = UID

data AnnTy  = AnnArrow AnnTy Ann AnnTy
            | AnyTy
            | AnnTyVar AnnTyVarId
            deriving (Show,Eq) 


meet :: Ann -> Ann -> Ann
Lazy   `meet` r       = r
r      `meet` Lazy    = r
Strict `meet` _       = Strict
_      `meet` Strict  = Strict
r1     `meet` r2      = Meet r1 r2

join :: Ann -> Ann -> Ann
Lazy   `join` _       = Lazy
_      `join` Lazy    = Lazy
Strict `join` r       = r
r      `join` Strict  = r
r1     `join` r2      = Join r1 r2

-- sacar?
getAnn :: AnnTy -> Ann
getAnn AnyTy                 = PhiEmpty  -- This could be a source of errors
getAnn (AnnArrow _ phi _)    = phi


isStrictAnn :: Ann -> Bool
isStrictAnn Strict = True
isStrictAnn _      = False

mkPhiVar :: AnnTyVarId -> Ann
mkPhiVar pvi = AnnVar pvi

anotateTy :: Ty -> AnnTy
anotateTy (Ty_App (Ty_App (Ty_Con cn) func) arg) 
                      | hsnIsArrow cn = AnnArrow (anotateTy func) PhiEmpty (anotateTy arg)
                      | otherwise     = AnyTy
anotateTy (Ty_Var tv categ) = AnnTyVar tv
anotateTy _ = AnyTy
{-
substAnnTy :: AnnTyVarId -> AnnTy -> AnnTy -> AnnTy
substAnnTy _  _  AnyTy                = AnyTy
substAnnTy tv nt (AnnArrow t1 phi t2) = AnnArrow (substAnnTy tv nt t1) phi (substAnnTy tv nt t2)
substAnnTy tv nt (AnnTyVar v)     
                          | tv == v   = nt
                          | otherwise = (AnnTyVar v)
-}
substAnnArrow :: AnnTy -> AnnTy -> AnnTy
substAnnArrow nt t@(AnnArrow t1 phi t2) = AnnArrow nt phi (substAnnTy t1 nt t2)


substAnnTy :: AnnTy -> AnnTy -> AnnTy -> AnnTy
substAnnTy ot nt st | ot == st  = nt
                    | otherwise = case st of
                                     AnnArrow l p r -> AnnArrow (substAnnTy ot nt l) p (substAnnTy ot nt r)
                                     _              -> st

{-
                                          (AnnTyVar v) -> substAnnTy v nt t
                                          _            -> (AnnArrow nt phi t2)    
substAnnArrow _  _                      = error "Strictness Analisis Error!!"
-}

%%]
