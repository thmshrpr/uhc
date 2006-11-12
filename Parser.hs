module Parser (parseLayer,parseImpl) where

import Lang
import UU.Parsing
import UU.Scanner


--------------------------------------------------------------------------------
-- LAYER PARSING
--------------------------------------------------------------------------------

lyrops  = [":", ","]
lyrkeys = [ "layer", "extends", "params", "uses", "pattern"
          , "in", "out", "inout", "node", "interface"]

-- TODO: return hierarchy 
parseLayer :: String -> IO Layer
parseLayer file = do { tokens <- scanFile	lyrkeys lyrops "{(:)}" "" (file ++ ".inf")
                     ; parseIO pLayer tokens
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
                   <*> pDirectives
   where mkParam io nm ty ds = Parameter_Parameter nm io ty (vis ds) (fltr ds)
         vis   ds            = not (elem "private" ds)
         fltr  ds            = filter (/= "private") ds

pDirection :: Parser Token Direction
pDirection =    (In <$ pKey "in")
            <|> (InOut <$ pKey "inout")
            <|> (Out <$ pKey "out")
            <|> (Node <$ pKey "node")

--------------------------------------------------------------------------------
-- IMPLEMENTATION PARSING
--------------------------------------------------------------------------------

implops  = [":.",";"]
implkeys = ["implementation","of","rule","ruleset","implements","pre","post", "where"]

parseImpl :: String -> IO Implementation
parseImpl file = do { tokens <- scanFile implkeys implops "{(:;)}=.|" "" (file ++ ".impl")
                    ; parseIO pImpl tokens
                    }

pImpl :: Parser Token Implementation
pImpl = Implementation_RawImplementation 
                  <$  pKey "implementation"
                  <*  pKey "of"
                  <*> pId
                  <*> pList pRuleSet

pRuleSet :: Parser Token RuleSet
pRuleSet = RuleSet_RuleSet <$  pKey "ruleset"
                           <*> pId 
                           <*  pKey "implements"
                           <*> pId
                           <*> pDirectives
                           <*> pList pRule

pRule :: Parser Token Rule
pRule = Rule_RawRule <$  pKey "rule"
                  <*> pVarid
                  <*  pKey "implements"
                  <*> pId
                  <*> opt (pKey "pre" *> pList pJudge) []
                  <* pKey "post" <*> pJudge

pJudge = pRawJudge1 <|> pRawJudge2

pRawJudge1 = mkJudge1 <$> pId 
                      <*  pKey "."
                      <*> pConid
                      <*  pKey "="
                      <*> pString
                      <*> pDefinitions
   where mkJudge1 int nm bdy defs = Judgment_RawJudgment1 nm int bdy defs

pRawJudge2 = mkJudge2 <$> pId 
                      <*  pKey "."
                      <*> pConid
                      <*> pList (pKey "|" *> pBinding)
                      <*> pDefinitions
   where mkJudge2 int nm bdy defs = Judgment_RawJudgment2 nm int bdy defs

pDirectives :: Parser Token [String]
pDirectives = (opt (pParens_pCommas pVarid) [])

pDefinitions :: Parser Token [(String,String)]
pDefinitions = opt (pKey "where" *> pList pBinding) []

pBinding :: Parser Token (String,String)
pBinding = (,) <$> pId <* pKey "=" <*> pString

pId :: Parser Token String
pId = pConid <|> pVarid
