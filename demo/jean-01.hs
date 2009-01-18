module Jean where

-- import Prelude ((+), (-), (==), IO, String, otherwise, print, Bool, Eq, False, True, ehcRunMain, fromInteger, return)

data List a = Cons a (List a) | Nil

drop' :: Int -> List a -> List a
drop' 0 xs = xs
drop' n Nil = Nil
drop' n (Cons x xs) = drop' (n-1) xs

length' :: List a -> Int
length' Nil = 0
length' (Cons x xs) = 1 + length' xs

droppedList :: List Int
droppedList = drop' 2 (Cons 1 (Cons 2 (Cons 3 Nil)))

main :: IO ()
main = print $ length' droppedList

