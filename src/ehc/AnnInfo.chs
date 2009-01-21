%%[1 module {%{EH}AnnInfo} 
%%]

%%[8 export(Ann(..),AnnTy(..),meet,join,getAnn,isStrictAnn,mkPhiVar,anotateTy,substAnnTy,substAnnArrow,getConstraints)
%%]

%%[8 import({%{EH}Base.Common}, {%{EH}Base.Builtin}, {%{EH}Ty})
%%]

%%[8
{-| PhiInfo is the information about the annotations 
-}
type PhiId = HsName -- UID

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

mkPhiVar :: PhiId -> Ann
mkPhiVar pvi = AnnVar pvi

anotateTy :: Ty -> UID -> (AnnTy,UID)
anotateTy (Ty_App (Ty_App (Ty_Con cn) func) arg) uid 
                      | hsnIsArrow cn = let (next,uniq) = mkNewUID uid
                                            (annTy1,next1) = anotateTy func next
                                            (annTy2,next2) = anotateTy arg  next1
                                        in  (AnnArrow annTy1 (AnnVar $ uidHNm uniq) annTy2, next2)
                      | otherwise     = (AnyTy, uid)
anotateTy (Ty_Var tv categ) uid       = (AnnTyVar tv, uid)
anotateTy _                 uid       = (AnyTy, uid)
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

getConstraints :: AnnTy -> AnnTy -> [(PhiId,Ann)]
getConstraints (AnnArrow ot1 (AnnVar phivar) ot2) (AnnArrow nt1 phi nt2) = (phivar,phi): 
                                                                           (  getConstraints ot1 nt1
                                                                           ++ getConstraints ot2 nt2 )
getConstraints _                                  _                      = []

{-
                                          (AnnTyVar v) -> substAnnTy v nt t
                                          _            -> (AnnArrow nt phi t2)    
substAnnArrow _  _                      = error "Strictness Analisis Error!!"
-}

%%]
