module Test where

import Lang

t = sem_Implementation test1

fitsIn_interface =
  Interface_Interface
    "fitsIn"
    [ Parameter_Parameter
        "guard"
        In
        "Bool"
        True
        []
    , Parameter_Parameter
        "typeLeft"
        In
        "Ty"
        True
        []
    , Parameter_Parameter
        "typeRight"
        In
        "Ty"
        True
        []
    , Parameter_Parameter
       "subst"
       Out
       "FIOut"
       True
       []
    ]
    [] -- no uses
    "g ==> typeLeft ~= typeRight"

checkSubsts_interface =
  Interface_Interface
    "checkSubs"
    [ Parameter_Parameter
        "iSubs"
        In
        "[FIOut]"
        True
        []
    , Parameter_Parameter
       "oSubs"
       Out
       "FIOut"
       True
       []
    ]
    [] -- no uses
    "iSubs ~> oSubs"


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
               "check_Subst"
               checkSubsts_interface
               []
               ( Judgment_Judgment
                 "R"
                 checkSubsts_interface
                 [ BodyAssignment_BodyAssignment (Parameter_Parameter "iSubs" In "[FIOut]" True []) "iSubs"
                 , BodyAssignment_BodyAssignment (Parameter_Parameter "oSubs" Out "FIOut"  True []) "foldr1  (\\fo1 fo2 -> if foHasErrs fo1 then fo1 else fo2) iSubs" 
                 ]
                 []
               )
            )  
          , (Rule_Rule
               "fitsIn_Ty_Con"
               fitsIn_interface
               []
               ( Judgment_Judgment
                 "R"
                 fitsIn_interface
                 [ BodyAssignment_BodyAssignment (Parameter_Parameter "guard" In "Bool" True []) "x == y"
                 , BodyAssignment_BodyAssignment (Parameter_Parameter "typeLeft" In "Ty" True []) "Ty_Con x"
                 , BodyAssignment_BodyAssignment (Parameter_Parameter "typeRight" In "Ty" True []) "Ty_Con y"
                 , BodyAssignment_BodyAssignment (Parameter_Parameter "subst" Out "FIOut" True []) "emptyFO {foTy = typeRight}"
                 ]
                 []
               )
             )
           , (Rule_Rule
               "fitsIn_Ty_Any_Left"
               fitsIn_interface
               []
               ( Judgment_Judgment
                 "R"
                 fitsIn_interface
                 [ BodyAssignment_BodyAssignment (Parameter_Parameter "typeLeft" In "Ty" True []) "Ty_Any"
                 , BodyAssignment_BodyAssignment (Parameter_Parameter "typeRight" In "Ty" True []) "typeRight"
                 , BodyAssignment_BodyAssignment (Parameter_Parameter "subst" Out "FIOut" True []) "emptyFO {foTy = typeRight}"
                 ]
                 []
               )
             )
          , (Rule_Rule
               "fitsIn_Ty_Any_Right"
               fitsIn_interface
               []
               ( Judgment_Judgment
                 "R"
                 fitsIn_interface
                 [ BodyAssignment_BodyAssignment (Parameter_Parameter "typeLeft" In "Ty" True []) "typeLeft"
                 , BodyAssignment_BodyAssignment (Parameter_Parameter "typeRight" In "Ty" True []) "Ty_Any"
                 , BodyAssignment_BodyAssignment (Parameter_Parameter "subst" Out "FIOut" True []) "emptyFO {foTy = typeLeft}"
                 ]
                 []
               )
             )
          , (Rule_Rule
               "fitsIn_Ty_App_Ty_App"
               fitsIn_interface
               [ Judgment_Judgment
                   "PreArgs"
                   fitsIn_interface
                   [ BodyAssignment_BodyAssignment (Parameter_Parameter "typeLeft" In "Ty" True [])  "ta2"
                   , BodyAssignment_BodyAssignment (Parameter_Parameter "typeRight" In "Ty" True []) "ta1"
                   , BodyAssignment_BodyAssignment (Parameter_Parameter "subst" Out "FIOut" True []) "afo"
                   ]
                   []
                 , Judgment_Judgment
                   "PreFun"
                   fitsIn_interface
                   [ BodyAssignment_BodyAssignment (Parameter_Parameter "typeLeft" In "Ty" True [])  "tr1"
                   , BodyAssignment_BodyAssignment (Parameter_Parameter "typeRight" In "Ty" True []) "tr2"
                   , BodyAssignment_BodyAssignment (Parameter_Parameter "subst" Out "FIOut" True []) "ffo"
                   ]
                   []
                 , Judgment_Judgment
                   "CheckSubs"
                   checkSubsts_interface
                   [ BodyAssignment_BodyAssignment (Parameter_Parameter "iSubs" In "[FIOut]" True []) "[ffo,afo,emptyFO {foTy = (\\a r -> [a] `mkArrow` r) (foTy ffo) (foTy afo)}]"
                   , BodyAssignment_BodyAssignment (Parameter_Parameter "oSubs" Out "FIOut"  True []) "res" 
                   ]
                   []
               ]
               ( Judgment_Judgment
                 "R"
                 fitsIn_interface
                 [ BodyAssignment_BodyAssignment (Parameter_Parameter "guard" In "Bool" True [])   "hsnIsArrow c1"
                 , BodyAssignment_BodyAssignment (Parameter_Parameter "guard" In "Bool" True [])   "c1 == c2"
                 , BodyAssignment_BodyAssignment (Parameter_Parameter "typeLeft" In "Ty" True [])  "Ty_App (Ty_App (Ty_Con c1) ta1) tr1"
                 , BodyAssignment_BodyAssignment (Parameter_Parameter "typeRight" In "Ty" True []) "Ty_App (Ty_App (Ty_Con c2) ta2) tr2"
                 , BodyAssignment_BodyAssignment (Parameter_Parameter "subst" Out "FIOut" True []) "res"
                 ]
                 []
               )
             )
          , (Rule_Rule
               "fitsIn_Ty_App"
               fitsIn_interface
               [ Judgment_Judgment
                   "PreArgs"
                   fitsIn_interface
                   [ BodyAssignment_BodyAssignment (Parameter_Parameter "typeLeft" In "Ty" True [])  "ta1"
                   , BodyAssignment_BodyAssignment (Parameter_Parameter "typeRight" In "Ty" True []) "ta2"
                   , BodyAssignment_BodyAssignment (Parameter_Parameter "subst" Out "FIOut" True []) "afo"
                   ]
                   []
                 , Judgment_Judgment
                   "PreFun"
                   fitsIn_interface
                   [ BodyAssignment_BodyAssignment (Parameter_Parameter "typeLeft" In "Ty" True [])  "tf1"
                   , BodyAssignment_BodyAssignment (Parameter_Parameter "typeRight" In "Ty" True []) "tf2"
                   , BodyAssignment_BodyAssignment (Parameter_Parameter "subst" Out "FIOut" True []) "ffo"
                   ]
                   []
                 , Judgment_Judgment
                   "CheckSubs"
                   checkSubsts_interface
                   [ BodyAssignment_BodyAssignment (Parameter_Parameter "iSubs" In "[FIOut]" True []) "[ffo,afo,emptyFO {foTy = Ty_App (foTy ffo) (foTy afo)}]"
                   , BodyAssignment_BodyAssignment (Parameter_Parameter "oSubs" Out "FIOut"  True []) "res" 
                   ]
                   []
               ]
               ( Judgment_Judgment
                 "R"
                 fitsIn_interface
                 [ BodyAssignment_BodyAssignment (Parameter_Parameter "typeLeft" In "Ty" True [])  "Ty_App tf1 ta1"
                 , BodyAssignment_BodyAssignment (Parameter_Parameter "typeRight" In "Ty" True []) "Ty_App tf2 ta2"
                 , BodyAssignment_BodyAssignment (Parameter_Parameter "subst" Out "FIOut" True []) "res"
                 ]
                 []
               )
             )
          , (Rule_Rule
               "fitsIn_Error"
               fitsIn_interface
               []
               ( Judgment_Judgment
                 "R"
                 fitsIn_interface
                 [ BodyAssignment_BodyAssignment (Parameter_Parameter "typeLeft" In "Ty" True [])  "typeLeft"
                 , BodyAssignment_BodyAssignment (Parameter_Parameter "typeRight" In "Ty" True []) "typeRight"
                 , BodyAssignment_BodyAssignment (Parameter_Parameter "subst" Out "FIOut" True []) "emptyFO {foErrL = [Err_UnifyClash typeLeft typeRight typeLeft typeRight]}"
                 ]
                 []
               )
             )
           ]
