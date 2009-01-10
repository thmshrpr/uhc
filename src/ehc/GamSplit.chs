%%[1 module {%{EH}GamSplit}
%%]

%%[8 export(gamSplit, gamContaintment)
%%]

%%[8 import({%{EH}Gam},{%{EH}AnnInfo})
%%]

%%[8
gamSplit :: ValGam -> ValGam -> ValGam
gamSplit = undefined
-- gamSplit (Gam ll) (Gam lr) = Gam (zipWith f ll lr) 
--     where f (ValGamInfo x info) (ValGamInfo x' info') 
--             | x == x'                                   = AssocL x $ ValGamInfo { vgiTy  = _vgiTy
--                                                                                 , vgiPhi =  meet _vgiPhi  _vgiPhi'
--                                                                                 }
--               where        _vgiTy                       = vgiTy info
--                            _vgiTy'                      = vgiTy info'
--                            _vgiPhi                      = vgiPhi info
--                            _vgiPhi'                     = vgiPhi info'
--             | otherwise                                 = error "This cannot happen here"

gamContaintment :: PhiInfo -> ValGam -> ValGam
gamContaintment phi gam     = gamMap f gam
    where f (x,info)        =  (x, info { vgiPhi = meet phi (vgiPhi info)})
          -- where       _tt   = vgiTy info
%%]