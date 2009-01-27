data Bool = False | True
data ''[]'' a = a : [a] | ''[]''

foreign import ccall "primEqInt"  (==) :: Int -> Int -> Bool
foreign import ccall "primSubInt" (-) :: Int -> Int -> Int

drop :: Int -> [Int] -> [Int]
drop n [] = []
drop n (x:xs) = drop (n-1) xs

main = drop 2 [1,2,3,4,5]

