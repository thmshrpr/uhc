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

%%[8 import(Data.Maybe, Data.List, Data.Monoid, Data.Array.ST, Control.Monad.ST, Control.Monad, EHCommon, GrinCode)
%%]

%%[8  import(UU.Pretty, GrinCodePretty) export("module Data.Monoid")
%%]


%%[8.OrdTag
instance Ord GrTag where
    compare t1 t2 = case t1 of
                        GrTag_Unboxed     -> case t2 of
                                                 GrTag_Unboxed     -> EQ
                                                 otherwise         -> LT
                        GrTag_Lit c1 _ n1 -> case t2 of
                                                 GrTag_Unboxed     -> GT
                                                 GrTag_Lit c2 _ n2 -> case compare c1 c2 of
                                                                          EQ -> compare n1 n2
                                                                          a  -> a
                                                 GrTag_Var _       -> LT
                        GrTag_Var n1      -> case t2 of
                                                 GrTag_Var n2      -> compare n1 n2
                                                 otherwise         -> GT
%%]

%%[8.AbstractValue export(AbstractValue(..), AbstractNode, Location, Variable)
data AbstractValue
  = AV_Nothing
  | AV_Basic
  | AV_Locations ![Location]
  | AV_Nodes ![AbstractNode]
  | AV_Error !String
	deriving (Eq, Ord)

instance Show AbstractValue where
    show av = case av of
                  AV_Nothing      -> "_|_"
                  AV_Basic        -> "{BAS}"
                  AV_Locations ls -> "{" ++ concatMap ((++", ").show) ls ++ "}"
                  AV_Nodes     ns -> "{" ++ concatMap (\(t,fs) -> show (ppGrTag' t) ++ show fs ++ ", ") ns ++ "}"
                  AV_Error     s  -> "E: " ++ s

type AbstractNode = (GrTag, [AbstractValue]) -- an AV_Nodes can not occur inside a AbstractNode
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
	                                  otherwise                          -> AV_Error $ "Wrong variable usage: Location, node or basic value mixed"

mergeLocations   al bl = sort $ union al bl -- TODO: painfully slow
mergeNodes an bn = let compareNode x y                = fst x == fst y
                       mergeNode1 nodes node@(nm,avs) = case h of
                                                         []        -> insert node nodes'
                                                         [(_,avs')] -> insert (nm, zipWith mappend avs' avs) nodes'
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
  | EnvUnion ![Variable] (Maybe AbstractEnvModifier) -- The Maybe contains only EnvSelect which is used for the apply function calls
  | EnvEval Variable
  | EnvLazyApp Variable [ApplyArg]
  | EnvApp Variable [ApplyArg]
  | EnvSelect Variable GrTag Int
  | EnvTag GrTag [Maybe Variable] (Maybe Variable)
	deriving (Show, Eq)

type ApplyArg = Either Variable AbstractEnvModifier -- only contains the EnvTag here

updateEnvElement :: AbstractEnvElement -> AbstractEnv s -> AbstractHeap s -> AssocL GrTag (Either GrTag Int) -> ST s AbstractEnvElement
updateEnvElement ee env heap applyMap = do
    { newChangeSet <- envChangeSet (aeMod ee) env heap applyMap
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
envChangeSet :: AbstractEnvModifier -> AbstractEnv s -> AbstractHeap s -> AssocL GrTag (Either GrTag Int) -> ST s AbstractValue
envChangeSet am env heap applyMap = case am of
                                        EnvNoChange        -> return AV_Nothing
                                        EnvUnion    vs mbS -> let addApplyArgument s av = envChangeSet s env heap applyMap >>= return . flip mappend av
                                                              in mapM valAbsEnv vs >>= return . mconcat >>= maybe return addApplyArgument mbS
                                        EnvEval     v      -> valAbsEnv v >>= evalChangeSet
                                        EnvApp      f a    -> do { pnodes  <- valAbsEnv f 
                                                                 ; argsVal <- mapM (\(Left v) -> valAbsEnv v) a
                                                                 ; applyChangeSet pnodes argsVal
                                                                 }
                                        EnvLazyApp  f a    -> return AV_Nothing -- valAbsEnv f >>= evalChangeSet >>= flip applyChangeSet a >> return AV_Nothing
                                        EnvSelect   v n i  -> valAbsEnv v >>= return . selectChangeSet n i
                                        EnvTag      t f r  -> tagChangeSet t f r
	where
    --valAbsEnv :: Variable -> ST s AbstractValue
    valAbsEnv v = do
        { elem <- lookupEnv env v
        ; return (aeBaseSet elem)
        }
    --valAbsHeap :: Location -> ST s AbstractValue
    valAbsHeap l = do
        { elem <- lookupHeap heap l
        ; return (ahBaseSet elem)
        }
    evalFilter (AV_Nodes nodes) = let isValueTag t = case t of
                                                         GrTag_Unboxed      -> True
                                                         GrTag_Lit cat _ _  -> case cat of
                                                                                   GrTagCon     -> True
                                                                                   GrTagPApp _  -> True
                                                                                   GrTagApp     -> False
                                                                                   GrTagFun     -> False
                                                                                   GrTagHole    -> False
                                                                                   GrTagRec     -> False
                                                         GrTag_Var _        -> error $ "tag variable unexpected: " ++ show t
                                      isValueNode (t,f) = isValueTag t
                                      newNodes          = filter isValueNode nodes
                                  in if null newNodes then AV_Nothing else AV_Nodes newNodes
    evalFilter av               = av
    --evalChangeSet :: AbstractValue -> ST s AbstractValue
    evalChangeSet av = case av of
                         AV_Nothing      -> return av
                         AV_Locations ls -> mapM valAbsHeap ls >>= return . mconcat . map evalFilter
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
    --applyChangeSet :: AbstractValue -> [AbstractValue] -> ST s AbstractValue
    applyChangeSet f argsVal = foldM applyChangeSet1 f argsVal
        where
        --applyChangeSet1 :: AbstractValue -> AbstractValue -> ST s AbstractValue
        applyChangeSet1 f arg = let partialApplicationNodes = [ node | node@((GrTag_Lit (GrTagPApp _) _ _), _) <- getNodes f ]
                                    --getNewNode :: GrTag -> [AbstractValue] -> ST s AbstractValue
                                    getNewNode tag args       = let newArgs = args ++ [arg]
                                                                in either (\tag' -> return $ AV_Nodes [(tag', newArgs)])
                                                                          (\var -> appendApplyArg env (AV_Nodes [(GrTag_Var (HNPos var), newArgs)]) >> valAbsEnv var)
                                                                          (fromJust' ("tag missing in applyMap: " ++ show tag) $ lookup tag applyMap)
                                in mapM (uncurry getNewNode) partialApplicationNodes >>= return . mconcat
%%]

%%[8 import(Data.Either) export(lookupEnv)
fromLeft  = either id                                     (const $ error "fromLeft: found right value")
fromRight = either (const $ error "fromRight: found left value") id

fromJust' s Nothing  = error $ "fromJust' Maybe:" ++ s
fromJust' _ (Just a) = a
--to break circular dependencies a copy paste from GRINCCommon:
getNodes av = case av of
                  AV_Nodes n  -> n
                  AV_Nothing  -> []
                  AV_Error s  -> error $ "analysis error: " ++  s
                  _           -> error $ "not a node: " ++ show av


lookupEnv :: AbstractEnv s -> Variable -> ST s AbstractEnvElement
lookupEnv env idx = readArray env idx

lookupHeap :: AbstractHeap s -> Location -> ST s AbstractHeapElement
lookupHeap heap idx = readArray heap idx

appendApplyArg :: AbstractEnv s -> AbstractValue -> ST s ()
appendApplyArg env av = let (applyArgIdx, _) = bounds env
                        in do { elem <- readArray env applyArgIdx
                              ; let applyArg = aeBaseSet elem
                                    newElem  = elem { aeBaseSet = av `mappend` applyArg }
                              ; writeArray env applyArgIdx newElem
                              }
%%]

%%[8.partialOrder
a <=! b = all (flip elem b) a
%%]

%%[8.fixpoint export(Label)
type Label       = Either Variable Location
type WorkList    = [Label]
type Depends     = (Label -> [Label])
type Analysis  s = (AbstractEnv s, AbstractHeap s)

{-
fixpoint :: Analysis s -> Depends -> WorkList -> (Label -> Analysis s -> ST s (Analysis s, Bool)) -> ST s (Analysis s)
fixpoint base deps []              f = return base
fixpoint base deps (work:worklist) f = do
    { (step, changed) <- f work base
    ; if changed
      then fixpoint step deps (deps work ++ worklist) f
      else fixpoint base deps worklist f
    }
-}

fixpoint labels f = countFixpoint 1
    where
    countFixpoint count = do
        { let doStep hasChanges l = f l >>= return . (hasChanges ||)
        ; changes <- foldM doStep False labels
        ; if changes
          then countFixpoint (count+1)
          else return count
        }
%%]

%%[8 export(heapPointsTo)
heapPointsTo :: AbstractEnv s -> AbstractHeap s -> AssocL GrTag (Either GrTag Int) -> ST s Int
heapPointsTo env heap applyMap =
    let labels        = heapLabels ++ envLabels
        heapLabels    = map Right (indices heap)
        envLabels     = map Left (tail $ indices env)
        f (Left i) = do
            { e  <- lookupEnv env i
            ; e' <- updateEnvElement e env heap applyMap
            ; let changed = isChanged (aeBaseSet e) (aeBaseSet e')
            ; when changed (writeArray env i e')
            ; return changed
            }
        f (Right i) = do
            { e  <- lookupHeap heap i
            ; e' <- updateHeapElement e env
            ; let changed = isChanged (ahBaseSet e) (ahBaseSet e')
            ; when changed (writeArray heap i e')
            ; return changed
            }
        -- @tracef l a = do { r@((env,heap), c) <- f l a
        --                ; msg <- if c then either (\i -> lookupEnv env i >>= return . show) (\i -> lookupHeap heap i >>= return . show) l else return "nothing"
        --                ; trace ("step: " ++ show l ++ " changed: " ++ show msg) $ return r
        --                }
    in fixpoint labels f


isChanged :: AbstractValue -> AbstractValue -> Bool
isChanged old new = old /= new
                    --case (old, new) of
                    --  (AV_Locations ol, AV_Locations nl) -> not $ nl <=! ol
                    --  (AV_Nodes     on, AV_Nodes nn    ) -> not $ nn <=! on
                    --  otherwise                          -> old /= new
		      
%%]

% vim:ts=4:et:ai:
