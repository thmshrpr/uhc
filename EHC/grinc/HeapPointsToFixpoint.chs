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
changeSet based on the previous values.

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

type Variable = HsName

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
mergeNodes an bn = let compareNode x y               = fst x == fst y
                       mergeNode1 nodes node@(nm,av) = case h of
                                                         []        -> node:nodes
                                                         [(_,av')] -> (nm, av' `mappend` av):nodes'
                                                         otherwise -> (nm, av `mappend` (mconcat $ map snd h)):nodes' -- should never be needed
                           where (h,nodes') = partition (compareNode node) nodes
                   in foldl' mergeNode1 an bn
%%]

%%[8.Heap export(AbstractHeap, AbstractHeapElement(..), AbstractHeapModifier, AbstractNodeModifier)
type AbstractHeap = [AbstractHeapElement]
data AbstractHeapElement = AbstractHeapElement
    { ahLabel     :: !Location
    , ahBaseSet   :: !AbstractValue
    , ahChangeSet :: !AbstractValue
    , ahMod       :: !AbstractHeapModifier
    }
	deriving (Eq)

instance Show AbstractHeapElement where
	show (AbstractHeapElement l b c m) =  "{" ++ show l ++ "}\n  " 
	                                   ++ "base   = " ++ show b ++ "\n  "
	                                   ++ "change = " ++ show c ++ "\n  "
	                                   ++ "mod    = " ++ show m ++ "\n"

type AbstractHeapModifier = (AbstractNodeModifier, Maybe Variable)
type AbstractNodeModifier = (GrTag, [Maybe Variable]) --(tag, [fields])

updateHeapElement :: AbstractHeapElement -> AbstractEnv -> AbstractHeapElement
updateHeapElement he env = let newBaseSet   = ahBaseSet he `mappend` ahChangeSet he
                               newChangeSet = heapChangeSet (ahMod he) env
                           in he { ahBaseSet = newBaseSet, ahChangeSet = newChangeSet }
%%]

%%[8.Environment export(AbstractEnv, AbstractEnvElement(..), AbstractEnvModifier(..))
type AbstractEnv = [AbstractEnvElement]
data AbstractEnvElement = AbstractEnvElement
    { aeLabel     :: !Variable
    , aeBaseSet   :: !AbstractValue
    , aeChangeSet :: !AbstractValue
    , aeMod       :: !AbstractEnvModifier
    }
	deriving (Eq)

instance Show AbstractEnvElement where
	show (AbstractEnvElement l b c m) =  "{" ++ show l ++ "}\n  " 
	                                   ++ "base   = " ++ show b ++ "\n  "
	                                   ++ "change = " ++ show c ++ "\n  "
	                                   ++ "mod    = " ++ show m ++ "\n"

data AbstractEnvModifier
  = EnvNoChange
  | EnvUnion ![Variable]
  | EnvEval Variable
  | EnvSelect Variable GrTag Int
  | EnvTag GrTag [Maybe Variable] (Maybe Variable)
	deriving (Show, Eq)

updateEnvElement :: AbstractEnvElement -> AbstractEnv -> AbstractHeap -> AbstractEnvElement
updateEnvElement ee env heap = let newBaseSet   = aeBaseSet ee `mappend` aeChangeSet ee
                                   newChangeSet = envChangeSet (aeMod ee) env heap
                               in ee { aeBaseSet = newBaseSet, aeChangeSet = newChangeSet }
%%]

%%[8.heapChangeSet
heapChangeSet :: AbstractHeapModifier -> AbstractEnv -> AbstractValue
heapChangeSet ((tag, deps), resultDep) env = AV_Nodes [(tag, getLocations deps)] `mappend` (maybe AV_Nothing (aeChangeSet . lookupEnv env) resultDep)
	where
	getLocations :: [Maybe Variable] -> [AbstractValue]
	getLocations  = map (maybe AV_Basic (aeChangeSet . lookupEnv env))
%%]

%%[8.envChangeSet
envChangeSet :: AbstractEnvModifier -> AbstractEnv -> AbstractHeap -> AbstractValue
envChangeSet am env heap = case am of
                             EnvNoChange     -> AV_Nothing
                             EnvUnion vs     -> mconcat $ map (aeChangeSet . lookupEnv env) vs
                             EnvEval  v      -> evalChangeSet (aeChangeSet $ lookupEnv env v)
                             EnvSelect v n i -> selectChangeSet (aeChangeSet $ lookupEnv env v) n i
                             EnvTag    t f r -> tagChangeSet t f r
	where
	evalChangeSet :: AbstractValue -> AbstractValue
	evalChangeSet av = case av of
	                     AV_Nothing      -> av
	                     AV_Locations ls -> foldl' (\r -> mappend r . ahChangeSet . lookupHeap heap) mempty ls
	                     AV_Error _      -> av
	                     otherwise       -> AV_Error "Variable passed to eval is not a location"
	selectChangeSet :: AbstractValue -> GrTag -> Int -> AbstractValue
	selectChangeSet av nm idx = case av of
	                              AV_Nothing    -> av
	                              AV_Nodes   ns -> lookup' ns nm !! idx
	                              AV_Error _    -> av
	                              otherwise     -> AV_Error "Variable passed to eval is not a node"
	tagChangeSet :: GrTag -> [Maybe Variable] -> (Maybe Variable) -> AbstractValue
	tagChangeSet t flds r = let toAbstract f (c, l) = let nothingVal = (c, AV_Basic:l) 
                                                              justFunc v = let changeSet = aeChangeSet (lookupEnv env v)
                                                                           in (c || isChanged changeSet, changeSet:l)
                                                          in maybe nothingVal justFunc f
                                    (varsChanged, vars) = foldr toAbstract (False, []) flds
                                    resVarNode          = r >>= return . lookupEnv env >>= return . aeChangeSet
                                    changed             = (maybe False isChanged resVarNode) || varsChanged
                                    newNodes            = AV_Nodes [(t, vars)]
                                in if changed then maybe newNodes (mappend newNodes) resVarNode else AV_Nothing
%%]

%%[8 export(lookupEnv)
lookupEnv :: AbstractEnv -> Variable -> AbstractEnvElement
lookupEnv env idx = case l of
                      []  -> error $ "Environment incomplete: '" ++ show idx ++ "' not found"
                      h:_ -> h
	where l = dropWhile ((idx /=) . aeLabel) env

lookupHeap :: AbstractHeap -> Location -> AbstractHeapElement
lookupHeap heap idx = case l of
                        []  -> error $ "Heap incomplete: '" ++ show idx ++ "' not found"
                        h:_ -> h
	where l = dropWhile ((idx /=) . ahLabel) heap

lookup' :: (Eq a) => [(a,b)] -> a -> b
lookup' list = fromJust . flip lookup list

isChanged :: AbstractValue -> Bool
isChanged AV_Nothing = False
isChanged _          = True
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
%%]

%%[8 export(heapPointsTo)
heapPointsTo :: AbstractEnv -> AbstractHeap -> Depends -> Analysis
heapPointsTo env heap deps = 
	let labels        = heapLabels . envLabels $ []
	    heapLabels    = foldl (\f e -> (Right (ahLabel e):) . f) id heap
	    envLabels     = foldl (\f e -> (Left  (aeLabel e):) . f) id env
	    f (Left i) (env,heap) = ((newE:rest,heap), changed)
	    	where
	    	([e], rest) = partition ((i==) . aeLabel) env
	    	newE        = updateEnvElement e env heap
	    	changed     = isChanged (aeChangeSet newE)
	    f (Right i) (env,heap) = ((env,newE:rest), changed)
	    	where
	    	([e], rest) = partition ((i==) . ahLabel) heap
	    	newE        = updateHeapElement e env
	    	changed     = isChanged (ahChangeSet newE)
	in fixpoint (env,heap) deps labels f
%%]
