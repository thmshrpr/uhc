module Ty.FitsIn
( fitsIn
, FIEnv (..), emptyFE )
where
import Base.Builtin
import Base.Common
import Ty.FitsInCommon
import Ty
import Error
import Cnstr
import Substitutable
import Ty.Instantiate
import Base.Opts
import Gam
import Data.Maybe
import Data.List as List
import Base.Debug



data FIIn   =  FIIn     {  fiFIOpts          ::  FIOpts              ,  fiUniq            ::  UID
                        }

emptyFI     =  FIIn     {  fiFIOpts          =   strongFIOpts        ,  fiUniq            =   uidStart
                        }

fiUpdOpts :: (FIOpts -> FIOpts) -> FIIn -> FIIn
fiUpdOpts upd fi = fi {fiFIOpts = upd (fiFIOpts fi)}

data FIEnv
  =   FIEnv

emptyFE
  =   FIEnv

instance Show FIEnv where
  show _ = "FIEnv"

manyFO :: [FIOut] -> FIOut
manyFO = foldr1 (\fo1 fo2 -> if foHasErrs fo1 then fo1 else fo2)

fitsIn :: FIOpts -> FIEnv -> UID -> Ty -> Ty -> FIOut
fitsIn opts env uniq ty1 ty2
  =  fo
  where
            res fi t                =  emptyFO  { foUniq = fiUniq fi, foTy = t
                                                , foAppSpineL = asGamLookup appSpineGam (tyConNm t)}
            err    e                =  emptyFO {foUniq = fioUniq opts, foErrL = e}
            errClash fi t1 t2       =  err [Err_UnifyClash ty1 ty2 (fioMode opts) t1 t2 (fioMode (fiFIOpts fi))]
            occurBind fi v t
                | v `elem` ftv t    =  err [Err_UnifyOccurs ty1 ty2 (fioMode opts) v t (fioMode (fiFIOpts fi))]
                | otherwise         =  bind fi v t

            bind fi tv t            =  (res fi t) {foCnstr = tv `cnstrTyUnit` t}

            allowBind fi (Ty_Var v f)   =  f == TyVarCateg_Plain

            allowImpredTVBindL fi t _
                = fioBindLFirst (fiFIOpts fi) && allowBind fi t
            allowImpredTVBindR fi t _
                = fioBindRFirst (fiFIOpts fi) && allowBind fi t

            unquant fi t hide howToInst
                =   (fi {fiUniq = u},uqt,back)
                where  (u,uq)         = mkNewLevUID (fiUniq fi)
                       (uqt,rtvs)     = tyInst1Quants uq howToInst t
                       back           = if hide  then  \fo ->  let  s = cnstrDel rtvs (foCnstr fo)
                                                               in   fo {foCnstr = s, foTy = s |=> t}
                                                 else  id
            foUpdTy  t   fo  = fo {foTy = t}
            foUpdCnstr c fo  = fo {foCnstr = c |=> foCnstr fo}

            foCmbAppTy   ffo afo  = afo {foTy = Ty_App (foCnstr afo |=> foTy ffo) (foTy afo)}
            foCmbCnstr   ffo afo  = afo {foCnstr = foCnstr afo |=> foCnstr ffo}
            foCmbCoCon   ffo afo  = afo {foAppSpineL = tail (foAppSpineL ffo)}

            foCmbApp     ffo      = foCmbCoCon ffo . foCmbCnstr ffo . foCmbAppTy ffo

            fPairWise' cCmb fi tL1 tL2
              =  foldr  (\(t1,t2) (foL,fii,c)
                           -> let  fo = ff (fii) (c |=> t1) (c |=> t2)
                              in   (fo:foL,fii {fiUniq = foUniq fo},fo `cCmb` c))
                        ([],fi,emptyCnstr)
                        (zip tL1 tL2)
            fPairWise = fPairWise' (\fo c -> foCnstr fo |=> c)

            ff fi t1 t2
              = f fi t1 t2
            f fi t1                     t2
                | fioMode (fiFIOpts fi) == FitSubRL = f  fi' t2 t1
                where  fi'       = fi  {fiFIOpts = fioSwapOpts . fioSwapCoCo ContraVariant . fiFIOpts $ fi}
            f fi Ty_Any                 t2          = res fi t2
            f fi t1                     Ty_Any      = res fi t1
            f fi t1@(Ty_Con s1)         t2@(Ty_Con s2)
                | s1 == s2                          = res fi t2
            f fi t1@(Ty_Var v1 f1)      t2@(Ty_Var v2 f2)
                | v1 == v2 && f1 == f2              = res fi t1
                | lBefR && allowBind fi t1          = bind fi v1 t2
                | not lBefR && allowBind fi t2      = bind fi v2 t1
                where lBefR = fioBindLBeforeR (fiFIOpts fi)

            f fi t1@(Ty_Var v1 _)       t2
                | allowImpredTVBindL fi t1 t2       = occurBind fi v1 t2
            f fi t1                     t2@(Ty_Var v2 _)
                | allowImpredTVBindR fi t2 t1       = occurBind fi v2 t1

            f fi t1@(Ty_Quant q1 _ _)   t2@(Ty_Quant q2 _ _)
                | fioMode (fiFIOpts fi) == FitUnify && q1 == q2
                                                    = ff fi2 uqt1 uqt2
                where  (fi1,uqt1,_) = unquant fi   t1 False instCoConst
                       (fi2,uqt2,_) = unquant fi1  t2 False instCoConst

            f fi t1                     t2@(Ty_Quant _ _ _)
                | fioIsSubsume (fiFIOpts fi) && fioLeaveRInst (fiFIOpts fi)
                                                    = back2 (ff fi2 t1 uqt2)
                where (fi2,uqt2,back2) = unquant fi t2 False instCoConst
            f fi t1                     t2@(Ty_Quant _ _ _)
                | fioIsSubsume (fiFIOpts fi) && not (fioLeaveRInst (fiFIOpts fi))
                                                    = back2 (ff fi2 t1 uqt2)
                where (fi2,uqt2,back2) = unquant fi t2 True instContra

            f fi t1@(Ty_Quant _ _ _)    t2
                | fioIsSubsume (fiFIOpts fi)        = ff fi1 uqt1 t2
                where (fi1,uqt1,back1) = unquant fi t1 False instCoConst

            f fi t1@(Ty_Var v1 _)       t2
                | allowBind fi t1                   = occurBind fi v1 t2
            f fi t1                     t2@(Ty_Var v2 _)
                | allowBind fi t2                   = occurBind fi v2 t1

            f fi t1@(Ty_App tf1 ta1)    t2@(Ty_App tf2 ta2)
                = manyFO [ffo,afo,foCmbApp ffo afo]
                where  ffo  = f fi tf1 tf2
                       fs   = foCnstr ffo
                       (as:_) = foAppSpineL ffo
                       fi'  = fi  { fiFIOpts  = asFIO as . fioSwapCoCo (asCoCo as) . fiFIOpts $ fi
                                  , fiUniq    = foUniq ffo }
                       afo  = f fi' (fs |=> ta1) (fs |=> ta2)

            f fi t1                     t2          = errClash fi t1 t2

            fo  = f (emptyFI {fiUniq = uniq, fiFIOpts = opts}) ty1 ty2

mkFitsInWrap' :: FIEnv -> FitsIn'
mkFitsInWrap' env
  =  \opt u t1 t2 ->  let  fo = fitsIn opt env u t1 t2
                      in   fo

mkFitsInWrap :: FIEnv -> FitsIn
mkFitsInWrap env
  =  \opt u t1 t2 ->  let  fo = fitsIn opt env u t1 t2
                      in   (foTy fo, foCnstr fo, foErrL fo)
