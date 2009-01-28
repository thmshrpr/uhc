data Bool = False | True
data ''[]'' a = a : [a] | ''[]''

foreign import ccall "primEqInt"  (==) :: Int -> Int -> Bool
foreign import ccall "primAddInt" (+) :: Int -> Int -> Int
foreign import ccall "primSubInt" (-) :: Int -> Int -> Int

undefined = False

drop :: Int -> [Int] -> [Int]
drop n [] = []
drop n (x:xs) | n == 0 = xs
              | True   = drop (n-1) (x:xs)

length :: [Int] -> Int
length [] = 0
length (x:xs) = 1 + length xs

main = length (drop 2 [1,2,3,4,5])

