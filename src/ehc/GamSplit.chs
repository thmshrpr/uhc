%%[1 module {%{EH}GamSplit}
%%]

%%[8 export(gamSplit, gamContaintment)
%%]

%%[8 import({%{EH}Gam},{%{EH}AnnInfo})
%%]

%%[8
gamSplit :: ValGam -> ValGam -> ValGam
gamSplit = gamZipWith f
    where f (x,info) (x',info') 
              | x == x'           = (x, info { vgiPhi = (vgiPhi info) `meet` (vgiPhi info') })
              | otherwise         = error "This cannot happen here"


gamContaintment :: PhiInfo -> ValGam -> ValGam
gamContaintment phi gam     = gamMap f gam
    where f (x,info)        =  (x, info { vgiPhi = phi `join` (vgiPhi info)})
%%]