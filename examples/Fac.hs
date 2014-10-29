
module Fac where

fac :: Int -> Int
fac 1 = 1
fac n = fac (n - 1) * n

main :: Int -> Int
main _ = fac 4

