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

%%[8 import(Data.Maybe, Data.List, Data.Monoid, Data.Array.ST, Control.Monad.ST, Control.Monad, EHCommon(HsName), GrinCode(GrTag))
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

%%[8.Heap export(AbstractHeap, AbstractHeapElement(..), AbstractHeapModifier, AbstractNodeModifier)
type AbstractHeap s = STArray s Location AbstractHeapElement
data AbstractHeapElement = AbstractHeapElement
    { ahBaseSet   :: !AbstractValue
    , ahMod       :: !AbstractHeapModifier
    }
	deriving (Eq)
	
instance Show AbstractHeapElement where
	show (AbstractHeapElement b m) = "base = " ++ show b ++ ";\t" ++ "mod = " ++ show m

type AbstractHeapModifier = (AbstractNodeModifier, Maybe Variable)
type AbstractNodeModifier = (GrTag, [Maybe Variable]) --(tag, [fields])

updateHeapElement :: AbstractHeapElement -> AbstractEnv s -> ST s AbstractHeapElement
updateHeapElement he env = do 
    { newChangeSet <- heapChangeSet (ahMod he) env
    ; let newBaseSet   = newChangeSet `mappend` ahBaseSet he
    ; return (he { ahBaseSet = newBaseSet })
    }
%%]

%%[8.Environment export(AbstractEnv, AbstractEnvElement(..), AbstractEnvModifier(..))
type AbstractEnv s = STArray s Variable AbstractEnvElement
data AbstractEnvElement = AbstractEnvElement
    { aeBaseSet   :: !AbstractValue
    , aeMod       :: !AbstractEnvModifier
    }
	deriving (Eq)

instance Show AbstractEnvElement where
	show (AbstractEnvElement b m) = "base = " ++ show b ++ ";\t" ++ "mod = " ++ show m

data AbstractEnvModifier
  = EnvNoChange
  | EnvUnion ![Variable]
  | EnvEval Variable
  | EnvSelect Variable GrTag Int
  | EnvTag GrTag [Maybe Variable] (Maybe Variable)
	deriving (Show, Eq)

updateEnvElement :: AbstractEnvElement -> AbstractEnv s -> AbstractHeap s -> ST s AbstractEnvElement
updateEnvElement ee env heap = do
    { newChangeSet <- envChangeSet (aeMod ee) env heap
    ; let newBaseSet = newChangeSet `mappend` aeBaseSet ee
    ; return (ee { aeBaseSet = newBaseSet })
    }
%%]

%%[8.heapChangeSet
heapChangeSet :: AbstractHeapModifier -> AbstractEnv s -> ST s AbstractValue
heapChangeSet ((tag, deps), resultDep) env = do
    { resCS <- getBaseSet resultDep
    ; locs  <- mapM getBaseSet deps
    ; return (AV_Nodes [(tag, locs)] `mappend` resCS)
    }
	where
    getBaseSet = maybe (return AV_Nothing) (\v -> lookupEnv env v >>= (return . aeBaseSet))
%%]

%%[8.envChangeSet
envChangeSet :: AbstractEnvModifier -> AbstractEnv s -> AbstractHeap s -> ST s AbstractValue
envChangeSet am env heap = case am of
                             EnvNoChange     -> return AV_Nothing
                             EnvUnion vs     -> mapM valAbsEnv vs >>= return . mconcat
                             EnvEval  v      -> valAbsEnv v >>= evalChangeSet
                             EnvSelect v n i -> valAbsEnv v >>= return . selectChangeSet n i
                             EnvTag    t f r -> tagChangeSet t f r
	where
    --valAbsEnv :: forall s. Variable -> ST s AbstractValue
    valAbsEnv v = do
        { elem <- lookupEnv env v
        ; return (aeBaseSet elem)
        }
    --valAbsHeap :: Location -> ST s AbstractValue
    valAbsHeap l = do
        { elem <- lookupHeap heap l
        ; return (ahBaseSet elem)
        }
    --evalChangeSet :: AbstractValue -> ST s AbstractValue
    evalChangeSet av = case av of
                         AV_Nothing      -> return av
                         AV_Locations ls -> mapM valAbsHeap ls >>= return . mconcat
                         AV_Error _      -> return av
                         otherwise       -> return $ AV_Error "Variable passed to eval is not a location"
    selectChangeSet :: GrTag -> Int -> AbstractValue -> AbstractValue
    selectChangeSet nm idx av = case av of
                                  AV_Nothing    -> av
                                  AV_Nodes   ns -> maybe AV_Nothing (!! idx) (lookup nm ns)
                                  AV_Error _    -> av
                                  otherwise     -> AV_Error "Variable passed to eval is not a node"
    --tagChangeSet :: GrTag -> [Maybe Variable] -> (Maybe Variable) -> ST s AbstractValue
    tagChangeSet t flds r = do { vars <- mapM (maybe (return AV_Basic) valAbsEnv) flds
                               ; let newNodes = AV_Nodes [(t, vars)]
                               ; maybe (return newNodes) (\v -> valAbsEnv v >>= return . mappend newNodes) r
                               }
%%]

%%[8 export(lookupEnv)
lookupEnv :: AbstractEnv s -> Variable -> ST s AbstractEnvElement
lookupEnv env idx = readArray env idx

lookupHeap :: AbstractHeap s -> Location -> ST s AbstractHeapElement
lookupHeap heap idx = readArray heap idx

lookup' :: (Eq a, Show a, Show b) => [(a,b)] -> a -> b
lookup' list e = fromJust' ("lookup: " ++ show e ++ " not found in " ++ show list) $ lookup e list

fromJust' _ (Just e)  = e
fromJust' s Nothing   = error $ "fromJust': " ++ s
%%]

%%[8.partialOrder
a <=! b = all (flip elem b) a
%%]

%%[8.fixpoint export(Label)
type Label       = Either Variable Location
type WorkList    = [Label]
type Depends     = (Label -> [Label])
type Analysis  s = (AbstractEnv s, AbstractHeap s)

fixpoint :: Analysis s -> Depends -> WorkList -> (Label -> Analysis s -> ST s (Analysis s, Bool)) -> ST s (Analysis s)
fixpoint base deps []              f = return base
fixpoint base deps (work:worklist) f = do
    { (step, changed) <- f work base
    ; if changed
      then fixpoint step deps (deps work ++ worklist) f
      else fixpoint base deps worklist f
    }
%%]

%%[8 import(Debug.Trace) export(heapPointsTo)
heapPointsTo :: AbstractEnv s -> AbstractHeap s -> ST s (Analysis s)
heapPointsTo env heap =
    let labels        = heapLabels ++ envLabels
        heapLabels    = map Right (indices heap)
        envLabels     = map Left (tail $ indices env)
        f (Left i) (env, heap) = do
            { e  <- readArray env i
            ; e' <- updateEnvElement e env heap
            ; let changed = isChanged (aeBaseSet e) (aeBaseSet e')
            ; when changed (writeArray env i e')
            ; return ((env,heap), changed)
            }
        f (Right i) (env, heap) = do
            { e  <- readArray heap i
            ; e' <- updateHeapElement e env
            ; let changed = isChanged (ahBaseSet e) (ahBaseSet e')
            ; when changed (writeArray heap i e')
            ; return ((env,heap), changed)
            }
        tracef w a = trace ("STEP: " ++ show w ++ "\n") f w a
    in fixpoint (env,heap) (const labels) labels f


isChanged :: AbstractValue -> AbstractValue -> Bool
isChanged old new = case (old, new) of
                      (AV_Locations ol, AV_Locations nl) -> not $ nl <=! ol
                      (AV_Nodes     on, AV_Nodes nn    ) -> not $ nn <=! on      -- will this be good enough?
		      otherwise                          -> old /= new
		      
%%]

% vim:ts=4:et:ai:
