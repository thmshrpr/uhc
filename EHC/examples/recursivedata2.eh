let data IntList = Nil | Cons Int IntList
in case Cons 3 Nil of
	Cons x _ -> x
