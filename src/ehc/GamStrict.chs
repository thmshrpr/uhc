%%[1 module {%{EH}GamSplit}
%%]

%%[8 export(gamSplit, gamContaintment)
%%]

%%[8 import({%{EH}Gam},{%{EH}AnnInfo})
%%]

%%[8
data ValGamInfoStrict
  = ValGamInfoStrict
      { vgiPhi :: PhiInfo       -- This contains the information about the strictness
      }
      deriving Show

type ValGamStrict = Gam HsNam ValGamInfoStrict


updateValPhiGamInfo :: PhiInfo -> HsName -> ValGam -> ValGam
updateValPhiGamInfo phi ns = gamMap f
    where f (ns',info)
              | ns == ns'  = (ns, info { vgiPhi = phi  })
              | otherwise  = (ns, info { vgiPhi = Lazy })

gamZipWith :: ((k,v) -> (k',v') -> (k'',v'')) -> Gam k v -> Gam k' v' -> Gam k'' v''
gamZipWith f (Gam ll) (Gam rl) = Gam (zipWith (zipWith f) ll rl)

gamDel :: Gam k v -> Gam k v
gamDel r@(Gam [])   = r
gamDel (Gam (l:ll)) = Gam ll 

isGamEmpty :: Gam k v -> Bool
isGamEmpty (Gam [[]])    = True
isGamEmpty (Gam (l:ll))  = null l

gamSplit :: ValGam -> ValGam -> ValGam
gamSplit = gamZipWith f
    where f (x,info) (x',info') 
              | x == x'           = (x, info { vgiPhi = (vgiPhi info) `meet` (vgiPhi info') })
              | otherwise         = error "This cannot happen here"


gamContaintment :: PhiInfo -> ValGam -> ValGam
gamContaintment phi gam     = gamMap f gam
    where f (x,info)        =  (x, info { vgiPhi = phi `join` (vgiPhi info)})
%%]