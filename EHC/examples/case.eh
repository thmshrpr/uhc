let data MaybeInt 
	= Just Int 
	| Nothing
in case Just 3 of
	Nothing -> 0
	Just x  -> x
