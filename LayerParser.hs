module LayerParser where

import Lang
import UU.Parsing
import UU.Scanner

{--

layer KnownTypes extends Equational
   interface Expr (e)
      params
         in  knType    : Ty 
      uses
         in     gamma  : Gamma
         inout  type   : Ty (retain)
      pattern {gamma ; type |- knType}

DATA Direction 
  | In 
  | Out 
  | InOut

-}

keywords   = ["layer", "extends", "params", "uses", "pattern"
             ,"in", "out", "inout"]
keyops = [":"]
specialchars = ""
opchars     = []

scnFile :: String -> IO [Token]
scnFile file = scanFile	keywords keyops specialchars opchars file


parseLayer :: String -> IO Layer
parseLayer file = do { tokens <- scnFile file 
                     ; parseIO pLayer tokens
                     }

pLayer :: Parser Token Layer
pLayer = Layer_RawLayer <$ pKey "layer" 
                        <*> pVarid
                        <*> opt (Just <$ pKey "extends" <*> pVarid) Nothing
                        <*> pList pInterface

pInterface :: Parser Token Interface
pInterface = Interface_Interface <$ pKey "interface"
                                    <*> pVarid
                                    <*> opt (Just <$> pParens pVarid) Nothing
                                    <*  pKey "params"
                                    <*> pList pParam
                                    <*> opt (pKey "uses" *> pList pParam) []
                                    <*  pKey "pattern" 
                                    <*> pBracks pString



pParam :: Parser Token Parameter
pParam = mkParam <$> pDirection 
                   <*> pVarid 
                   <*  pKey ":" 
                   <*> pConid 
                   <*> (opt (pParens_pCommas pVarid) [])

mkParam dir name ty directives = Parameter_Parameter name dir ty ("private" `elem` directives) directives

pDirection :: Parser Token Direction
pDirection =    (Direction_In <$ pKey "in")
            <|> (Direction_InOut <$ pKey "inout")
            <|> (Direction_Out <$ pKey "out")