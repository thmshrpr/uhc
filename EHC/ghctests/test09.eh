let data Either = I Int | C Char
in case I 1 of
	I a -> a
	C _ -> 0
