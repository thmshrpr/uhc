let data MaybeInt = INothing | IJust Int
    data MaybeMaybe = MNothing | MJust MaybeInt
in case MJust (IJust 3) of
	MJust INothing -> 0
	MJust (IJust y) -> y

