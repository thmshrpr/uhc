module Test where

import Lang

expr_interface =
  Interface_Interface 
    "Expr"
    [ Parameter_Parameter
        "e"
        Node
        "Expr"
        True
        []
    , Parameter_Parameter
        "valGam"
        In
        "ValGam"
        True
        []                
    , Parameter_Parameter
        "tyGam"
        In
        "TyGam"
        True
        []
    , Parameter_Parameter
        "kiGam"
        In
        "KiGam"
        True
        []
    , Parameter_Parameter
        "ty"
        InOut
        "Ty"
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
              ( Judgment_Judgment
                  "R"
                  expr_interface
                  [ BodyAssignment_BodyAssignment (Parameter_Parameter "kiGam" In "KiGam" True []) "kiGam"
                  , BodyAssignment_BodyAssignment (Parameter_Parameter "tyGam" In "TyGam" True []) "tyGam"
                  , BodyAssignment_BodyAssignment (Parameter_Parameter "valGam" In "ValGam" True []) "valGam"
                  , BodyAssignment_BodyAssignment (Parameter_Parameter "e" Node "Expr" True []) "IConst"
                  , BodyAssignment_BodyAssignment (Parameter_Parameter "ty" InOut "Ty" True []) "tyInt"
                  ]
                  []
              )
             )
           ]          