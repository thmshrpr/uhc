let data Either = Left Int | Right Int
in case Left 5 of
	(Left a) -> a
	(Right a) -> a
