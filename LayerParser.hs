module LayerParser where

import Lang
import UU.Parsing
import UU.Scanner

lyrops  = [":"]
lyrkeys = ["layer","extends","params","uses","pattern","in","out","inout","interface"]

main :: IO ()
main = do { layer <- parseLayer "Equation.inf"
          ; putStr $ show layer
          ; impl <- parseImpl "Equation.impl"
          ; putStr $ show impl
          }

parseLayer :: String -> IO Layer
parseLayer file = do { tokens <- scanFile	lyrkeys lyrops "{(:)}" "" file
                     ; parseIO pLayer tokens
                     }

pLayer :: Parser Token Layer
pLayer = Layer_RawLayer <$ pKey "layer" 
                        <*> pConid
                        <*> opt (Just <$ pKey "extends" <*> pConid) Nothing
                        <*> pList pInterface


pInterface :: Parser Token Interface
pInterface = Interface_Interface <$ pKey "interface"
                                    <*> pConid
                                    <*> opt (Just <$> pParens pVarid) Nothing
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

{-------
implementation of Equational
  rule fitsIn implements Expr
    pre
    post R : Expr = "kiGam ; tyGam ; valGam :- int : tyInt"
-------}

implops  = [":","="]
implkeys = ["implementation","of","rule","implements","pre","post"]

parseImpl :: String -> IO Implementation
parseImpl file = do { tokens <- scanFile implkeys implops "{(:)}=" "" file
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
                  <* pKey "post" <*> pList pJudge

pJudge = Judgment_RawJudgment <$> pConid 
                              <*  pKey ":"
                              <*> pConid
                              <*  pKey "="
                              <*> pString
                              <*> pSucceed []




