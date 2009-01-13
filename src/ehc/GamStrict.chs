%%[1 module {%{EH}GamStrict}
%%]

%%[8 import({%{EH}Gam},{%{EH}AnnInfo},{%{EH}Base.Common})
%%]

%%[8 export(gamSplit,gamContaintment)
gamSplit :: ValGamStrict -> ValGamStrict -> ValGamStrict
gamSplit = gamZipWith f
    where f (x,info) (x',info') 
              | x == x'           = (x, info { vgiPhi = (vgiPhi info) `meet` (vgiPhi info') })
              | otherwise         = error "This cannot happen here"


gamContaintment :: PhiInfo -> ValGamStrict -> ValGamStrict
gamContaintment phi gam     = gamMap f gam
    where f (x,info)        =  (x, info { vgiPhi = phi `join` (vgiPhi info)})
%%]