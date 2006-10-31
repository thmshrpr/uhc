module LayerParser where

import Lang
import UU.Parsing
import UU.Scanner

lyrops  = [":", ","]
lyrkeys = [ "layer", "extends", "params", "uses", "pattern"
          , "in", "out", "inout", "node", "interface"]

main :: IO ()
main = do { layer <- parseLayer "Equation.inf"
          ; putStr $ show layer
          ; putStrLn ""
          ; putStrLn ""
          ; impl <- parseImpl "Equation.impl"
          ; putStr $ show impl
          }

parseLayer :: String -> IO Layer
parseLayer file = do { tokens <- scanFile	lyrkeys lyrops "{(:)}" "" file
                     ; layer  <- parseIO pLayer tokens
                     ; checkLayerName file layer
                     ; resolveParents layer
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
   = do parent <- parseLayer $ p ++ ".inf"
        return $ Layer_Layer n (Just parent) is

checkLayerName :: String -> Layer -> IO Layer
checkLayerName nm l@(Layer_RawLayer n p is) = if n == nm then return l else error msg
   where msg = "Layer file: " ++ nm ++ " contains layer with name: " ++ n

checkParamAccess :: Layer -> IO Layer
checkParamAccess l@(Layer_Layer n Nothing is) = return l
checkParamAccess l@(Layer_Layer n Nothing is) 
   = undefined

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
