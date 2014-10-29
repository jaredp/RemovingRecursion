-- Try "ghc -ddump-ds FibTR.hs"

module FibTR ( fib ) where

import Data.Int ( Int8, Int32 )

data Stack = Fib2 Int8 Stack  -- Int8 is n (live variable)
           | Fib3 Int32 Stack -- Int32 is the result from fib (n-1)
           | BOS
                
data Action = Call    Int8       -- The outermost call (no stack)
            | Recurse Int8 Stack -- Int8 is argument passed to fib by itself
            | Return  Int32 Stack-- Int32 is return value from fib
               
-- fibh :: (Eq a, Eq b, Num a, Num b) => Action a b -> b

fibh (Call    n)                    = fibh (Recurse n     BOS)
fibh (Recurse 1     s)              = fibh (Return  1     s)
fibh (Recurse 2     s)              = fibh (Return  1     s)
fibh (Recurse n     s)              = fibh (Recurse (n - 1) (Fib2 n s))
fibh (Return  n     BOS)            = n
fibh (Return  fibn1 (Fib2 n s))     = fibh (Recurse (n - 2) (Fib3 fibn1 s))
fibh (Return  fibn2 (Fib3 fibn1 s)) = fibh (Return  (fibn1 + fibn2) s)
--{-# INLINE fibh #-}

fib :: Int8 -> Int32
fib n = fibh (Call n)
