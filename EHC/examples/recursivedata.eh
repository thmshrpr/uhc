let data IntList = Nil | Cons Int IntList
    head = \l -> case l of
                     Cons x _ -> x
in head (Cons 3 Nil)
