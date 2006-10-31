module LayerParser where

import Lang
import UU.Parsing
import UU.Scanner
import Debug.Trace

lyrops  = [":", ","]
lyrkeys = [ "layer", "extends", "params", "uses", "pattern"
          , "in", "out", "inout", "node", "interface"]

main :: IO ()
main = do { layer <- parseLayer "Equation"
          ; putStr $ show layer
          ; putStrLn ""
          ; putStrLn ""
          ; impl <- parseImpl "Equation.impl"
          ; putStr $ show impl
          }

parseLayer :: String -> IO Layer
parseLayer file = do { tokens <- scanFile	lyrkeys lyrops "{(:)}" "" (file ++ ".inf")
                     ; layer  <- parseIO pLayer tokens
                     ; checkLayerName file layer
                     ; player <- resolveParents layer
                     ; checkParamAccess player
                     ; return player
                     }

pLayer :: Parser Token Layer
pLayer = Layer_RawLayer <$ pKey "layer" 
                        <*> pConid
                        <*> opt (Just <$ pKey "extends" <*> pConid) Nothing
                        <*> pList pInterface

pInterface :: Parser Token Interface
pInterface = Interface_Interface <$ pKey "interface"
                                    <*> pId
                                    <*  pKey "params"
                                    <*> pList pParam
                                    <*> opt (pKey "uses" *> pList pParam) []
                                    <*  pKey "pattern" 
                                    <*> pString

pParam :: Parser Token Parameter
pParam = mkParam <$> pDirection 
                   <*> pVarid 
                   <*  pKey ":" 
                   <*> pConid 
                   <*> (opt (pParens_pCommas pVarid) [])
   where mkParam io nm ty ds = Parameter_Parameter nm io ty (vis ds) (fltr ds)
         vis   ds            = not (elem "private" ds)
         fltr  ds            = filter (/= "private") ds

pDirection :: Parser Token Direction
pDirection =    (In <$ pKey "in")
            <|> (InOut <$ pKey "inout")
            <|> (Out <$ pKey "out")
            <|> (Node <$ pKey "node")

----------------- LAYER POST PROCESSING -----------------

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
visibleParams = layerParams isVisible isVisible
   where isVisible (Parameter_Parameter _ _ _ v _) = v

usedParams :: Layer -> [(String,[String])]
usedParams = layerParams (const False) (const True)

layerParams :: ParamFilter -> ParamFilter -> Layer -> [(String,[String])]
layerParams pf uf l = map (\i->(iname i, tracer $ params pf uf i)) (ifaces l)
   where iname (Interface_Interface nm pa us p) = nm

ifaces :: Layer -> [Interface]
ifaces (Layer_Layer n p is) = is


params :: ParamFilter -> ParamFilter -> Interface -> [String]
params pf uf (Interface_Interface n ps us p) = map toName $ filter pf ps ++ filter uf us
   where toName (Parameter_Parameter n d t v ds) = n

tracer = (\x -> trace (show x) x) 

dictionary xs nm = if null x then error "not in dictionary: " ++ nm else head x
   where x = [r | (n,r) <- xs, n == nm]

----------------- IMPLEMENTATION POST PROCESSING -----------------

type ImplAlg = ( String -> Layer               -- name to layer
               , String -> Interface           -- name to interface
               , String -> String -> Parameter -- name & name to param
               )

foldImplementation :: ImplAlg -> Implementation -> Implementation
foldImplementation (n2l, n2i, nn2p) = f0
   where f0 (Implementation_RawImplementation nm rs)  
             = Implementation_Implementation (n2l nm) (map f1 rs)
         f1 (Rule_RawRule nm ni pres post) 
             = Rule_Rule nm (n2i ni) (map f2 pres) (f2 post)
         f2 (Judgment_RawJudgment2 nm ni bdy defs) 
             = Judgment_Judgment nm (n2i ni) (map (f3 ni) bdy) defs
         f3 ni (pnm,expr) = BodyAssignment_BodyAssignment (nn2p ni pnm) expr


hierarchy :: Layer -> [(String, Layer)]
hierarchy l@(Layer_Layer nm Nothing is) = [(nm,l)]
hierarchy l@(Layer_Layer nm (Just p) is) = hierarchy p ++ [(nm,l)]

interfaces :: Layer -> [(String, Interface)]
interfaces l = map pairify $ ifaces l
   where pairify i@(Interface_Interface n ps us p) = (n,i)

parameters :: Layer -> [(String, Parameter)]
parameters = undefined
--layerParams (const True) (const True)
-- postProcess [(String, String)]

-- dictionary 

-------------------------------------------------------------------------------

implops  = [":.",";"]
implkeys = ["implementation","of","rule","implements","pre","post", "where"]

parseImpl :: String -> IO Implementation
parseImpl file = do { tokens <- scanFile implkeys implops "{(:;)}=.|" "" file
                    ; parseIO pImpl tokens
                    }

pImpl :: Parser Token Implementation
pImpl = Implementation_RawImplementation 
                  <$  pKey "implementation"
                  <*  pKey "of"
                  <*> pConid
                  <*> pList pRule

pRule :: Parser Token Rule
pRule = Rule_RawRule <$  pKey "rule"
                  <*> pVarid
                  <*  pKey "implements"
                  <*> pConid
                  <*> opt (pKey "pre" *> pList pJudge) []
                  <* pKey "post" <*> pJudge

pJudge = pRawJudge1 <|> pRawJudge2

pRawJudge1 = mkJudge1 <$> pConid 
                                   <*  pKey "."
                                   <*> pConid
                                   <*  pKey "="
                                   <*> pString
                                   <*> pDefinitions
   where mkJudge1 int nm bdy defs = Judgment_RawJudgment1 nm int bdy defs

pRawJudge2 = mkJudge2 <$> pConid 
                                   <*  pKey "."
                                   <*> pConid
                                   <*> pList (pKey "|" *> pBinding)
                                   <*> pDefinitions
   where mkJudge2 int nm bdy defs = Judgment_RawJudgment2 nm int bdy defs

pDefinitions :: Parser Token [(String,String)]
pDefinitions = opt (pKey "where" *> pList pBinding) []

pBinding :: Parser Token (String,String)
pBinding = (,) <$> pId <* pKey "=" <*> pString

pId :: Parser Token String
pId = pConid <|> pVarid
