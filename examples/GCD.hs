module GCD where

mygcd :: Int -> Int -> Int
mygcd a b | a == b    = a
          | a < b     = mygcd a (b - a)
          | otherwise = mygcd (a - b) b
          
main :: Int -> Int
main _ = mygcd 140 64

