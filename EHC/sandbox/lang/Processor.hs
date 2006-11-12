module Processor where

import Lang
import Debug.Trace
import Data.List
import Parser


main :: IO ()
main = do { impls <- compile "Equation"
          ; putStr $ show impls
          }

compile :: String -> IO Implementation
compile name = do rawlayer <- parseLayer name
                  target   <- processLayer name rawlayer 
                  let layers = hierarchy target
                  impls  <- resolveImpls layers
                  return $ head impls ------------------ foldImpls   


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
   = do parent <- parseLayer p
        return $ Layer_Layer n (Just parent) is

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
         msg ba = "you lose" ++ show ba
   

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
layerParams pf uf l = map (\i->(name i, tracer $ params pf uf i)) (ifaces l)

ifaces :: Layer -> [Interface]
ifaces (Layer_Layer n p is) = is


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
         f1b (Rule_RawRule nm ni pres post) 
             = Rule_Rule nm (n2i ni) (map f2 pres) (f2 post)
         f2 (Judgment_RawJudgment2 nm ni bdy defs) 
             = Judgment_Judgment nm (n2i ni) (map (f3 ni) bdy) defs
         f3 ni (pnm,expr) = BodyAssignment_BodyAssignment (nn2p ni pnm) expr


hierarchy :: Layer -> [Layer]
hierarchy l@(Layer_Layer _ Nothing _) = [l]
hierarchy l@(Layer_Layer _ (Just p) _) = hierarchy p ++ [l]

parameters :: Layer -> [(String, Parameter)]
parameters = undefined


resolve :: Layer -> Implementation -> IO Implementation
resolve l i = return $ foldImpl (a,b,c) i
   where a = getR $ hierarchy l
         b = getR $ ifaces l
         c pi p2 = get p2 $ get' pi (allParams l)
         allParams = layerParams (const True) (const True)

linkLayer :: Layer -> IO [Implementation]
linkLayer l 
   = do let hier = hierarchy l
        let intsd = getR $ ifaces l
        let alg = (getR hier, intsd, undefined)
        return undefined



-- TODO: check judgment LHS names are visible
-- TODO: check rule names don't appear twice in file
-- TODO: check that impl name matches file name

resolveImpls :: [Layer] -> IO [Implementation]
resolveImpls []     = return []
resolveImpls (l:ls) = do impl <- parseImpl (name l) 
                         impl <- resolve l impl
                         impls <- resolveImpls ls
                         return $ impl : impls

-- combining layers:

zipImpls :: Implementation -> Implementation -> Implementation
zipImpls (Implementation_Implementation l rs1) 
         (Implementation_Implementation _ rs2)
   = Implementation_Implementation l $ zipRuleSets rs1 rs2

zipRuleSets = undefined
-- TODO: assumes we checked for (erroneous duplication of rules in file) 

zipRules :: [Rule] -> [Rule] -> [Rule]
zipRules above  []    = above
zipRules []     below = below
zipRules (x:xs) below = makeOne x match : zipRules xs nomatch
   where (match,nomatch) = partition (matchRule x) below
         makeOne r [] = r
         makeOne r rs = mergeRule r (head rs)

mergeRule :: Rule -> Rule -> Rule
mergeRule (Rule_Rule nm i pre1 post1) 
          (Rule_Rule _ _ pre2 post2)
   = Rule_Rule nm i (mergeJudges pre1 pre2) (mergeJudge post1 post2) 

mergeJudges j1 j2 = undefined

mergeJudge = undefined

matchRule :: Rule -> Rule -> Bool
matchRule (Rule_Rule n1 i1 _ _) (Rule_Rule n2 i2 _ _) = n1 == n2 && i1 == i2


--------------------------------------------------------------------------------
-- UTILITY FUNCTIONS
--------------------------------------------------------------------------------

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

