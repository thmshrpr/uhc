module Test where

import Lang

expr_interface =
  Interface_Interface 
    "Expr"
    (Just "e")
    [ Parameter_Parameter
        "valGam"
        Direction_In
        "ValGam"
        True
        []                
    , Parameter_Parameter
        "tyGam"
        Direction_In
        "TyGam"
        True
        []
    , Parameter_Parameter
        "kiGam"
        Direction_In
        "KiGam"
        True
        []
    ]
    []      -- no uses
    "kiGam ; tyGam ; valGam :- e : ty"   -- pattern    

test1 = Implementation_Implementation
          (Layer_Layer      -- shortcut the Layer, we don't pretty print that 
            "Equational"
            Nothing 
            [ expr_interface ]
          )
          [ (Rule_Rule
              "fitsIn"
              expr_interface
              []
              [ Judgment_Judgment
                  "R"
                  expr_interface
                  "kiGam ; tyGam ; valGam :- int : tyInt"
                  []
              ]
             )
           ]          