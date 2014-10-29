

import IR
import RemoveRecursionPass ( 
    tail_recursion_pass, module_recursiveness_stats
 )
import IRInterpreter ( runMain )
import IRTransforms
import IRToHaskell

import Outputable

nt = IntType
k = Literal . IntLiteral

plus = Call nt "+"
times = Call nt "*"
minus = Call nt "-"
fac = Call nt "fac"
x = Var nt "x"

v = Var nt "a"
c x = Case (Var (TypeRef "D") "a") "c" [(LabelMatch "F" [("f", nt), ("a", nt)], x)]
e = Let [("a", Var nt "v"), ("b", Var nt "x")]

a = (c $ e v)
b = (e $ c v)

s = putStrLn . showSDoc . (ppr :: Expr -> SDoc)

{-
facir = Function nt [(nt, "x")] $
	Let [
	  ("res", 
		Case x "_" [
			(LiteralMatch $ IntLiteral 1, k 1),
			(DefaultMatch, fac [minus [x, k 1]])
		  ]
	   )
	] $ times [x, Var nt "res"]

mainir = Function nt [(nt, "_")] $
	fac [k 4]
	
strty = SumType [("C", [IntType]), ("E", [])]

strlit (x:xs) = SumLiteral (TypeRef "String") "C" [IntLiteral x, strlit xs]
strlit [] = SumLiteral (TypeRef "String") "E" []

ir = Module "" ([("fac", facir), ("main", mainir)], [("String", strty)])
tmod = tail_recursion_pass ir

s = module_recursiveness_stats ir
s' = module_recursiveness_stats tmod

m = runMain ir
m' = runMain tmod
-}
