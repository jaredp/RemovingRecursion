

module Tr where

import Prelude ((+), (-), (*), (<), (==), Bool, Int)

{- 
 s3 -> s4 is generally how we could combine mutually recursive fns.
 Stage 3 is mutually recursive if there are multiple recursive calls in the
 same control flow path [eg: (fib (n-1)) + (fib (n-2)) ].
-}

-- sandbox
{-
data Bool = True | False

(&&) :: Bool -> Bool -> Bool
infixl 3 &&

True && True = True
_ && _ = False

(||) :: Bool -> Bool -> Bool
infixl 2 ||

False || False = False
_ || _ = True

not :: Bool -> Bool
not True = False
not False = True

(==) :: Int -> Int -> Bool
infixl 4 ==
x == y = diff (x - y)
	where
	diff 0 = True
	diff _ = False
	
(/=) :: Int -> Int -> Bool
infixl 4 /=
x /= y = not (x == y)


--

data IntList = Cons Int IntList | Empty


{-
scan :: Int -> IntList
scan 0 = Empty
scan x = Cons (go x) (scan (x-1))
-}


a :: Int
a = 13

b :: Int
b = a * 2 - 20


ifB :: Bool -> Bool -> Bool -> Bool
ifB True t _ = t
ifB False _ f = f

ifI :: Bool -> Int -> Int -> Int
ifI q t f = case q of
	True -> t
	False -> f


(<) :: Int -> Int -> Bool
infixl 4 <
x < y = test (x - y) 0
	where
	test q d = 
		case q + d of
            0 -> True
            _ -> (
                case q - d of
                    0 -> False
                    _ -> test q (d+1)
              )


gcd :: Int -> Int -> Int
gcd a b 
   | a == b = a
   | b < a = gcd (a - b) b
   | a < b = gcd a (b - a)
-}

main :: Int -> Int
main _ = fib3 6

-- fib/fac

{-
fac1 :: Int -> Int
fac1 1 = 1
fac1 n = fac1 (n - 1) * n

fib1 :: Int -> Int
fib1 1 = 1
fib1 2 = 1
fib1 n = fib1 (n - 1) + fib1 (n-2)
-}
-- stage 3

data Fib3cT = Fib3cTA Int Fib3cT 
			| Fib3cTB Int Fib3cT
			| Fib3cTC

fib3c :: Int -> Fib3cT -> Int
fib3c x (Fib3cTA n c) = fib3' (n-2) (Fib3cTB x c)
fib3c y (Fib3cTB x c) = fib3c (x + y) c
fib3c x Fib3cTC = x

fib3' :: Int -> Fib3cT -> Int
fib3' 1 c = fib3c 1 c
fib3' 2 c = fib3c 1 c
fib3' n c = fib3' (n-1) (Fib3cTA n c)

fib3 n = fib3' n Fib3cTC 
{-
-- stage 4 fib/fac

data Fac4cT = Fac4cTA Int Fac4cT | Fac4cTB
data Fac4T = Fac4r Int Fac4cT | Fac4c Int Fac4cT

fac4' :: Fac4T -> Int
fac4' (Fac4r 1 c) = fac4' (Fac4c 1 c)
fac4' (Fac4r n c) = fac4' (Fac4r (n - 1) (Fac4cTA n c))
fac4' (Fac4c x (Fac4cTA n c)) = fac4' (Fac4c (n * x) c)
fac4' (Fac4c x Fac4cTB) = x

fac4 :: Int -> Int
fac4 n  = fac4' (Fac4r n Fac4cTB)


data Fib4cT = Fib4cTA Int Fib4cT 
			| Fib4cTB Int Fib4cT
			| Fib4cTC
data Fib4T = Fib4r Int Fib4cT | Fib4c Int Fib4cT

fib4' :: Fib4T -> Int
fib4' (Fib4r 1 c) = fib4' (Fib4c 1 c)
fib4' (Fib4r 2 c) = fib4' (Fib4c 1 c)
fib4' (Fib4r n c) = fib4' (Fib4r (n - 1) (Fib4cTA n c))
fib4' (Fib4c x (Fib4cTA n c)) = fib4' (Fib4r (n - 2) (Fib4cTB x c))
fib4' (Fib4c y (Fib4cTB x c)) = fib4' (Fib4c (x + y) c)
fib4' (Fib4c x Fib4cTC) = x

fib4 n = fib4' (Fib4r n Fib4cTC)
-}

