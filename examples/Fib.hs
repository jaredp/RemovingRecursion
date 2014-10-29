
module Fib where

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

main :: Int -> Int
main _ = fib 4

