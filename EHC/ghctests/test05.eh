let f :: Int -> Int
    g :: Int -> Int
    f = \x -> g x
    g = \x -> f x
in f 1
