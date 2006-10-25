module LayerParser where

import Lang
import UU.Parsing
import UU.Scanner

ops  = [":"]
keys = ["layer","extends","params","uses","pattern","in","out","inout","interface"]

main :: IO ()
main = do { layer <- parseLayer "Equation.inf"
          ; putStr $ show layer
          }

parseLayer :: String -> IO Layer
parseLayer file = do { tokens <- scanFile	keys ops "{(:)}" "" file
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
pDirection =    (Direction_In <$ pKey "in")
            <|> (Direction_InOut <$ pKey "inout")
            <|> (Direction_Out <$ pKey "out")
