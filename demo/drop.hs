data Bool = False | True
data ''[]'' a = a : [a] | ''[]''

foreign import ccall "primEqInt"  (==) :: Int -> Int -> Bool
foreign import ccall "primAddInt" (+) :: Int -> Int -> Int
foreign import ccall "primSubInt" (-) :: Int -> Int -> Int

undefined = False

staart :: Int -> [Int] -> [Int]
staart n [] = []
staart n (x:xs) | n == 0 = xs
                | True   = staart (n-1) (x:xs)

som :: [Int] -> Int
som [] = 0
som (x:xs) = 1 + som xs

main = som (staart 2 [1,2,3,4,5])

