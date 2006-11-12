module Processor where

import Lang
import Debug.Trace
import Data.List
import Parser


--------------------------------------------------------------------------------
-- LAYER POST PROCESSING
--------------------------------------------------------------------------------

processLayer :: String -> Layer -> IO Layer
processLayer file layer = do { checkLayerName file layer
                             ; player <- resolveParents layer
                             ; checkParamAccess player
                             ; return player
                             }

resolveParents :: Layer -> IO Layer
resolveParents (Layer_RawLayer n Nothing is) = return $ Layer_Layer n Nothing is
resolveParents (Layer_RawLayer n (Just p) is) 
   = do parent    <- parseLayer p
        processed <- processLayer p parent
        return $ Layer_Layer n (Just processed) is

checkLayerName :: String -> Layer -> IO Layer
checkLayerName nm l@(Layer_RawLayer n p is) = if n == nm then return l else error msg
   where msg = "Layer file: " ++ nm ++ " contains layer with name: " ++ n


checkParamAccess :: Layer -> IO ()
checkParamAccess child@(Layer_Layer n Nothing is) = return ()
checkParamAccess child@(Layer_Layer n (Just parent) is) 
   = checkSubset (usedParams child) (visibleParams parent)
   where checkSubset us vs = let ba = badaccess us vs in
                                 if null $ ba
                                 then return () 
                                 else error $ msg ba
         badaccess us vs = foldr (++) [] (zipWith notin us (repeat vs))
         msg ba = "Access Error: " ++ show ba
   

notin (i,ps) vis = map (\x -> i ++ "." ++ x) [p | p <- ps, not (elem p vs)]
   where vs = foldr (++) [] [xs | (i',xs) <- vis, i' == i]
   
type ParamFilter = Parameter -> Bool


visibleParams :: Layer -> [(String,[String])]
visibleParams l = ps2n $ layerParams isVisible isVisible l
   where isVisible (Parameter_Parameter _ _ _ v _) = v
         ps2n = map (\(n,ps) -> (n, map p2n ps) )
         p2n (Parameter_Parameter n d t v ds) = n

usedParams :: Layer -> [(String,[String])]
usedParams l = ps2n $ layerParams (const False) (const True) l
   where ps2n = map (\(n,ps) -> (n, map p2n ps) )
         p2n (Parameter_Parameter n d t v ds) = n


layerParams :: ParamFilter -> ParamFilter -> Layer -> [(String,[Parameter])]
layerParams pf uf l = map (\i->(name i, params pf uf i)) (ifaces l)

ifaces :: Layer -> [Interface]
ifaces (Layer_Layer n p is) = is
ifaces (Layer_RawLayer n p is) = is

params :: ParamFilter -> ParamFilter -> Interface -> [Parameter]
params pf uf (Interface_Interface n ps us p) = filter pf ps ++ filter uf us

--------------------------------------------------------------------------------
-- IMPLEMENTATION POST PROCESSING
--------------------------------------------------------------------------------

type ImplAlg = ( String -> Layer               -- name to layer
               , String -> Interface           -- name to interface
               , String -> String -> Parameter -- name & name to param
               )

foldImpl :: ImplAlg -> Implementation -> Implementation
foldImpl (n2l, n2i, nn2p) = f0
   where f0 (Implementation_RawImplementation nm rs)  
             = Implementation_Implementation (n2l nm) (map f1a rs)
         f1a (RuleSet_RuleSet nm ni ds rs)
             = RuleSet_RuleSet nm ni ds $ map f1b rs
         f1b (Rule_RawRule nm ni ds pres post) 
             = Rule_Rule nm (n2i ni) ds (map f2 pres) (f2 post)
         f2 (Judgment_RawJudgment2 nm ni ds bdy defs) 
             = Judgment_Judgment nm (n2i ni) ds (map (f3 ni) bdy) defs
         f3 ni (pnm,expr) = BodyAssignment_BodyAssignment (nn2p ni pnm) expr


hierarchy :: Layer -> [Layer]
hierarchy l@(Layer_Layer _ Nothing _) = [l]
hierarchy l@(Layer_Layer _ (Just p) _) = hierarchy p ++ [l]

resolve :: Layer -> Implementation -> IO Implementation
resolve l i = return $ foldImpl (a,b,c) i
   where a = getR $ hierarchy l
         b = getR $ ifaces l
         c pi p2 = get p2 $ get' pi (allParams l)
         allParams = layerParams (const True) (const True)



-- TODO: check judgment LHS names are visible
-- TODO: check rule names don't appear twice in file
-- TODO: check that impl name matches file name

resolveImpls :: [Layer] -> IO [Implementation]
resolveImpls []     = return []
resolveImpls (l:ls) = do impl <- parseImpl (name l) 
                         impl <- resolve l impl
                         impls <- resolveImpls ls
                         return $ impl : impls

--------------------------------------------------------------------------------
-- MERGING IMPLEMENTATION LAYERS
--------------------------------------------------------------------------------

merge :: [Implementation] -> Implementation
merge (x:xs) = foldl mergeImpls x xs

mergeImpls :: Implementation -> Implementation -> Implementation
mergeImpls (Implementation_Implementation l rs1) 
           (Implementation_Implementation _ rs2)
   = Implementation_Implementation l $ mergeRuleSets rs1 rs2

mergeRuleSets :: RuleSets -> RuleSets -> RuleSets
mergeRuleSets = mergePreserveOrder mergeRuleSetPair

mergeRuleSetPair :: RuleSet -> RuleSet -> RuleSet
mergeRuleSetPair r1@(RuleSet_RuleSet n i d rs1)
                 r2@(RuleSet_RuleSet _ _ _ rs2)
   = if hasDirective r1 "overwrite"
     then r2
     else RuleSet_RuleSet n i d $ mergePreserveOrder mergeRulePair rs1 rs2

mergeRulePair :: Rule -> Rule -> Rule
mergeRulePair r1@(Rule_Rule n i ds pre1 post1) 
              r2@(Rule_Rule _ _ _  pre2 post2) 
   = if hasDirective r1 "overwrite"
     then r1
     else Rule_Rule n i ds pre post
   where pre  = mergePreserveOrder mergeJudgmentPair pre1 pre2
         post = mergeJudgmentPair post1 post2


mergeJudgmentPair :: Judgment -> Judgment -> Judgment
mergeJudgmentPair j1@(Judgment_Judgment n i ds bdy1 defs1) 
                  j2@(Judgment_Judgment _ _ _  bdy2 defs2)
   = if j1 /= j2 || hasDirective j1 "overwrite"
     then j1 
     else Judgment_Judgment n i ds bdy defs
   where bdy  = mergeAssignments bdy1 bdy2
         defs = if hasDirective j1 "overwrite_defs" 
                then defs1
                else mergeDefinitions defs1 defs2


mergeAssignments = mergePreserveOrder const

mergeDefinitions = mergePreserveOrder' eqFst const


mergePreserveOrder :: (Eq a, Show a) => (a -> a -> a) -> [a] -> [a] -> [a]
mergePreserveOrder = mergePreserveOrder' (==)

mergePreserveOrder' :: (Eq a, Show a) => (a -> a -> Bool) 
                            -> (a -> a -> a) 
                            -> [a] -> [a] -> [a]
mergePreserveOrder' e m []     bs     = bs
mergePreserveOrder' e m as     []     = as
mergePreserveOrder' e m (a:as) (bs)   = if not $ elemBy e a bs
                                        then a : mergePreserveOrder m as bs
                                        else neq' ++ (m a match : mergeRest)

   where (neq,eq)   = span (\x -> not $ e a x) bs
         match      = head eq
         (neq',eq') = break (`elem` as) neq
         mergeRest = mergePreserveOrder' e m as (eq' ++ tail eq)

--------------------------------------------------------------------------------
-- UTILITY FUNCTIONS
--------------------------------------------------------------------------------

tracer :: Show a => a -> a
tracer = (\x -> trace (show x) x) 

get :: Named a => String -> [a] -> a
get nm xs = if null fs then err else head fs
   where fs  = [x| x <- xs, name x == nm]
         err = error $ "value with name: " ++ nm ++ " was not found."

get' :: String -> [(String,a)] -> a
get' nm xs = if null fs then err else head fs
   where fs  = [x| (n,x) <- xs, n == nm]
         err = error $ "value with name: " ++ nm ++ " was not found."

getR :: Named a => [a] -> String -> a
getR = flip get

elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy eq x xs = not $ null [y | y <- xs, eq x y]

eqFst :: Eq a => (a,a) -> (a,a) -> Bool
eqFst (a,b) (c,d) = a == c
