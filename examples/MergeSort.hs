module MergeSort where

data IntList = Cons Int IntList | Empty -- deriving (Show)

-----------------------------------

mergesort :: IntList -> IntList
mergesort l = splitforsort Empty Empty l True

splitforsort :: IntList -> IntList -> IntList -> Bool -> IntList
splitforsort lhs rhs Empty _ = mergesort' lhs rhs
splitforsort lhs rhs (Cons x xs) addToLhs =
	if addToLhs
	then splitforsort (Cons x lhs) rhs xs False
	else splitforsort lhs (Cons x rhs) xs True

mergesort' :: IntList -> IntList -> IntList
mergesort' Empty Empty = Empty
-- the following cases should only be hit if l has one element, and is thus sorted
mergesort' Empty l = l 
mergesort' l Empty = l
mergesort' lhs rhs = merge (mergesort lhs) (mergesort rhs)

-- parameters are sorted
merge :: IntList -> IntList -> IntList
merge l Empty = l
merge Empty l = l
merge lhs@(Cons l ls) rhs@(Cons r rs) =
	Cons gt (merge lhs' rhs')
	where
	lgt = l > r
	gt = if lgt then l else r
	lhs' = if lgt then ls else lhs
	rhs' = if lgt then rhs else rs

-----------------------------------

main :: Int -> IntList
main _ = mergesort (Cons 4 (Cons 3 (Cons 9 (Cons 12 (Cons 0 (Cons 2 Empty))))))

