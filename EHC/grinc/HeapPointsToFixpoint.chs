% $Id$
%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

We want to have two mappings. The eval mapping and the apply mapping.

The eval mapping consist of a mapping from Tag (with arity information) to a Grin function, or the keyword unit
The apply mapping consist of a mapping from  tag to tag or to a grin function

examples:
eval:
FInt(1) -> unit
Fupto(2) -> upto
Fsum(2)  -> sum

apply:
Pupto_2(0) -> Pupto_1
Pupto_1(1) -> upto

Now we retrieve the equations and initial environment and heap:

- Every variable gets a number
- The equations are represented by a base set a change set and an eqution part

On every iteration we merge the changeSet with the BaseSet and define the new
changeSet based on the previous values. This is only possible if we update all equations each iteration.

Note: we can implement the heap and environment as an array. With constant
lookup times.

%%[8 import(Data.Maybe, Data.List, Data.Monoid, EHCommon(HsName), GrinCode(GrTag))
%%]

%%[8.AbstractValue export(AbstractValue(..), AbstractNode, Location, Variable)
data AbstractValue
  = AV_Nothing
  | AV_Basic
  | AV_Locations ![Location]
  | AV_Nodes ![AbstractNode]
  | AV_Error !String
	deriving (Show, Eq)

type AbstractNode = (GrTag, [AbstractValue]) -- Of course a Nodes can no occur inside a AbstractNode
type Location = Int

type Variable = Int

instance Monoid AbstractValue where
	mempty  = AV_Nothing
	mappend a          AV_Nothing = a
	mappend AV_Nothing b          = b
	mappend a          b          = case (a,b) of
	                                  (AV_Basic       , AV_Basic       ) -> AV_Basic
	                                  (AV_Locations al, AV_Locations bl) -> AV_Locations (al `mergeLocations` bl)
	                                  (AV_Nodes     an, AV_Nodes     bn) -> AV_Nodes (an `mergeNodes` bn)
	                                  (AV_Error     _ , _              ) -> a
	                                  (_              , AV_Error     _ ) -> b
	                                  otherwise                          -> AV_Error "Wrong variable usage: Location, node or basic value mixed"

mergeLocations   = union
mergeNodes an bn = let compareNode x y                = fst x == fst y
                       mergeNode1 nodes node@(nm,avs) = case h of
                                                         []        -> node:nodes'
                                                         [(_,avs')] -> (nm, zipWith mappend avs' avs):nodes'
                                                         otherwise -> error "Multiple nodes with the same tag found"  --- should never occur
                           where (h,nodes') = partition (compareNode node) nodes
                   in foldl' mergeNode1 an bn
%%]

%%[8.Heap export(AbstractHeap, AbstractHeapElement(..), AbstractHeapModifier, AbstractNodeModifier, ahLabel)
type AbstractHeap = [(Location, AbstractHeapElement)]
data AbstractHeapElement = AbstractHeapElement
    { ahBaseSet   :: !AbstractValue
    , ahMod       :: !AbstractHeapModifier
    }
	deriving (Eq)
	
ahLabel = fst


instance Show AbstractHeapElement where
	show (AbstractHeapElement b m) = "\nbase = " ++ show b ++ "\t;\t" ++ "mod = " ++ show m ++ "\n"

type AbstractHeapModifier = (AbstractNodeModifier, Maybe Variable)
type AbstractNodeModifier = (GrTag, [Maybe Variable]) --(tag, [fields])

updateHeapElement :: AbstractHeapElement -> AbstractEnv -> AbstractHeapElement
updateHeapElement he env = let newChangeSet = heapChangeSet (ahMod he) env
                               newBaseSet   = newChangeSet `mappend` ahBaseSet he
                           in he { ahBaseSet = newBaseSet }
%%]

%%[8.Environment export(AbstractEnv, AbstractEnvElement(..), AbstractEnvModifier(..),aeLabel)
type AbstractEnv = [(Variable, AbstractEnvElement)]
data AbstractEnvElement = AbstractEnvElement
    { aeBaseSet   :: !AbstractValue
    , aeMod       :: !AbstractEnvModifier
    }
	deriving (Eq)

aeLabel = fst	

instance Show AbstractEnvElement where
	show (AbstractEnvElement b m) = "\nbase = " ++ show b ++ "\t;\t" ++ "mod = " ++ show m ++ "\n"

data AbstractEnvModifier
  = EnvNoChange
  | EnvUnion ![Variable]
  | EnvEval Variable
  | EnvSelect Variable GrTag Int
  | EnvTag GrTag [Maybe Variable] (Maybe Variable)
	deriving (Show, Eq)

updateEnvElement :: AbstractEnvElement -> AbstractEnv -> AbstractHeap -> AbstractEnvElement
updateEnvElement ee env heap = let newChangeSet = envChangeSet (aeMod ee) env heap
                                   newBaseSet   = newChangeSet `mappend` aeBaseSet ee
                               in ee { aeBaseSet = newBaseSet }
%%]

%%[8.heapChangeSet
heapChangeSet :: AbstractHeapModifier -> AbstractEnv -> AbstractValue
heapChangeSet ((tag, deps), resultDep) env = AV_Nodes [(tag, getLocations deps)] `mappend` (maybe AV_Nothing (aeBaseSet . lookupEnv env) resultDep)
	where
	getLocations :: [Maybe Variable] -> [AbstractValue]
	getLocations  = map (maybe AV_Basic (aeBaseSet . lookupEnv env))
%%]

%%[8.envChangeSet
envChangeSet :: AbstractEnvModifier -> AbstractEnv -> AbstractHeap -> AbstractValue
envChangeSet am env heap = case am of
                             EnvNoChange     -> AV_Nothing
                             EnvUnion vs     -> mconcat $ map (aeBaseSet . lookupEnv env) vs
                             EnvEval  v      -> evalChangeSet (aeBaseSet $ lookupEnv env v)
                             EnvSelect v n i -> selectChangeSet (aeBaseSet $ lookupEnv env v) n i
                             EnvTag    t f r -> tagChangeSet t f r
	where
	evalChangeSet :: AbstractValue -> AbstractValue
	evalChangeSet av = case av of
	                     AV_Nothing      -> av
	                     AV_Locations ls -> foldl' (\r -> mappend r . ahBaseSet . lookupHeap heap) mempty ls
	                     AV_Error _      -> av
	                     otherwise       -> AV_Error "Variable passed to eval is not a location"
	selectChangeSet :: AbstractValue -> GrTag -> Int -> AbstractValue
	selectChangeSet av nm idx = case av of
	                              AV_Nothing    -> av
	                              AV_Nodes   ns -> lookup' ns nm !! idx
	                              AV_Error _    -> av
	                              otherwise     -> AV_Error "Variable passed to eval is not a node"
	tagChangeSet :: GrTag -> [Maybe Variable] -> (Maybe Variable) -> AbstractValue
	tagChangeSet t flds r = let toAbstract f = let nothingVal = AV_Basic
                                                       justFunc v = aeBaseSet (lookupEnv env v)
                                                   in maybe nothingVal justFunc f
                                    vars = map toAbstract flds
                                    resVarNode          = r >>= return . lookupEnv env >>= return . aeBaseSet
                                    newNodes            = AV_Nodes [(t, vars)]
                                in maybe newNodes (mappend newNodes) resVarNode
%%]

%%[8 export(lookupEnv)
lookupEnv :: AbstractEnv -> Variable -> AbstractEnvElement
lookupEnv env idx = case l of
                      []  -> error $ "Environment incomplete: '" ++ show idx ++ "' not found"
                      h:_ -> h
	where l = maybe [] (\e -> [e]) (lookup idx env)

lookupHeap :: AbstractHeap -> Location -> AbstractHeapElement
lookupHeap heap idx = case l of
                        []  -> error $ "Heap incomplete: '" ++ show idx ++ "' not found"
                        h:_ -> h
	where l = maybe [] (\e -> [e]) (lookup idx heap)

lookup' :: (Eq a) => [(a,b)] -> a -> b
lookup' list = fromJust . flip lookup list

%%]

%%[8.partialOrder
a <=! b = all (flip elem b) a
%%]

%%[8.fixpoint export(Label)
type Label     = Either Variable Location
type WorkList  = [Label]
type Depends   = (Label -> [Label])
type Analysis  = (AbstractEnv, AbstractHeap)

fixpoint :: Analysis -> Depends -> WorkList -> (Label -> Analysis -> (Analysis, Bool)) -> Analysis
fixpoint base deps []              f = base
fixpoint base deps (work:worklist) f | changed   = fixpoint step deps (deps work ++ worklist) f
                                     | otherwise = fixpoint base deps worklist f
	where (step, changed) = f work base

{-
fixpoint :: Analysis -> Depends -> WorkList -> (Label -> Analysis -> (Analysis, Bool)) -> Analysis
fixpoint base deps []       f = base
fixpoint base deps worklist f = let step work            =  let (newElem, changed) = f work base
	                                                    in (newElem, if changed then deps work else [])
                                    (analysis, depsList) = unzip (map step worklist)
                                    newWork              = concat depsList
                                in fixpoint analysis deps newWork f
-}

%%]

%%[8 import(Debug.Trace) export(heapPointsTo)
heapPointsTo :: AbstractEnv -> AbstractHeap -> Depends -> Analysis
heapPointsTo env heap deps = 
	let labels        = heapLabels . envLabels $ []
	    heapLabels    = foldl (\f e -> (Right (ahLabel e):) . f) id heap
	    envLabels     = foldl (\f e -> (Left  (aeLabel e):) . f) id env
	    f (Left i) (env,heap) = (((i,newE):rest,heap), changed)
	    	where
	    	(el, rest)  = partition ((i==) . aeLabel) env
		[(_,e)]     = if length el /= 1 then error (show el) else el
	    	newE        = updateEnvElement e env heap
	    	changed     = isChanged (aeBaseSet e) (aeBaseSet newE)
	    f (Right i) (env,heap) = ((env,(i,newE):rest), changed)
	    	where
	    	(el, rest)  = partition ((i==) . ahLabel) heap
		[(_,e)]    = if length el /= 1 then error (show el) else el
	    	newE        = updateHeapElement e env
	    	changed     = isChanged  (ahBaseSet e) (ahBaseSet newE)
	    tracef w a = trace ("STEP: " ++ show w ++ "\n" ++ show a) f w a
	in fixpoint (env,heap) deps labels f


isChanged :: AbstractValue -> AbstractValue -> Bool
isChanged old new = case (old, new) of
                      (AV_Locations ol, AV_Locations nl) -> not $ nl <=! ol
                      (AV_Nodes     on, AV_Nodes nn    ) -> not $ nn <=! on      -- will this be good enough?
		      otherwise                          -> old /= new
		      
%%]
