module SumList where

data IntList = Cons Int IntList | Empty

mysum :: IntList -> Int
mysum (Cons x xs) = x + mysum xs
mysum Empty = 0

main :: Int -> Int
main _ = mysum (Cons 4 (Cons 3 (Cons 9 (Cons 12 (Cons 0 (Cons 2 Empty))))))

