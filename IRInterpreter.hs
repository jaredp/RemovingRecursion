
module IRInterpreter where

import IR
import IRUtils

import IRToHaskell
import Outputable ( showSDoc )

import Data.List as List

-----------

-- the return type is ignored by interpret

runMain :: Module -> Literal
runMain m = interpret_no_stackoverflow ns mainCall
	where
	ns = mnamespace m
	mainCall = Call IntType "main" args
	args = [Literal $ SumLiteral "GHC.Types.Int" "GHC.Types.I#" [IntLiteral 0]]

-----------

type Scope = [(String, Literal)]

{-
TODO: add SumLiteral vars to globals before running
TODO: add fns, primOps, and ctors into [(String, [Literal] -> Literal)] before running
-}

interpret_no_stackoverflow :: Namespace -> Expr -> Literal
interpret_no_stackoverflow = interpret Nothing

interpret :: Maybe Int -> Namespace -> Expr -> Literal
interpret maxstack (fns, tys) = interpret' (0, "") []
	where
	-- Constructors and special variables
	construct :: Type -> String -> [Literal] -> Maybe Literal
	construct (TypeRef tname) ctor args = do
		SumType s <- lookup tname tys
		let ctors = map fst s
		just_if (ctor `elem` ctors) $ SumLiteral tname ctor args
	construct _ _ _ = Nothing
	
	-- Interpret (stackdepth, backtrace) scope expression
	interpret' :: (Int, String) -> Scope -> Expr -> Literal

	-- Literal
	interpret' _ _ (Literal l) = l
    
	-- Let expression
	-- Adds variables to scope
	interpret' bt scope (Let bindings expr) =
		interpret' bt (newscope ++ scope) expr
		where newscope = map_assoc (interpret' bt scope) bindings


	-- Variable lookup
	-- data X = A | B => when A is used, it is Var (TypeRef "X") "A"
	interpret' bt scope (Var t vname) = 
		first_found [var, label, error not_found]
		where
		var = List.lookup vname scope
		label = construct t vname []

		not_found = fail_msg $ "not found: " ++ vname
		fail_msg msg = snd bt ++ "\n\n" ++ msg
		
		
	-- Function calls - these can be standard functions, 
	-- constructors, or primitive operations (+, -, *)
	interpret' bt scope (Call t' fname callArgs) =
		first_found [stack_overflow, standardRet, ctorRet, primRet, not_found]
		where
		--------------------------------------
		-- common call features
		argValues = map (interpret' bt scope) callArgs

		--------------------------------------
		-- error handling
		newbt = (fst bt + 1, snd bt
			++ "\n\n>" ++ fname
			++ "\n" ++ showSDoc (pprType t')
			++ "\n" ++ show (map (showSDoc . pprExpr) callArgs)
			++ "\n" ++ show (map (showSDoc . pprLiteral) argValues))
		fail_msg msg = snd newbt ++ "\n\n" ++ msg
		failmsg = error . fail_msg
		
		stack_overflow = do
		    overflow <- maxstack
		    just_if (fst bt > overflow) (failmsg "stack overflow")
		
		not_found = failmsg "invalid function call"
		
		---------------------------------------		
		-- regular function call
		standardRet = do
			Function _ args impl <- List.lookup fname fns
			let callvars = zip (map snd args) argValues
			return $ interpret' newbt callvars impl
			
		---------------------------------------		
		-- constructor iff fname is a label
		ctorRet = construct t' fname argValues
		
		---------------------------------------
		-- primitive operation (+, -, *)
		primRet = do
			primop <- List.lookup fname primOps
			return $ primop fail_msg argValues
		
		
	-- Case expression
	interpret' bt scope (Case matching v patterns) =
		first_found $ List.map (match val) (List.sort patterns) ++ [error not_found]
		where
		val = interpret' bt scope matching
		newscope = (v, val) : scope
		
		not_found = fail_msg $
		    "unmatched: " ++ (showSDoc $ pprLiteral val) 
            {- ++ " among " ++ show (map (showSDoc . pprPattern) patterns) -}
		fail_msg msg = snd bt ++ "\n\n" ++ msg
	
		--pattern matching
		match :: Literal -> (Pattern, Expr) -> Maybe Literal

		match _ (DefaultMatch, expr) = 
			Just $ interpret' bt newscope expr
		
		match l (LiteralMatch lp, expr) =
			just_if (l == lp) $ interpret' bt newscope expr
		
		match (SumLiteral _ slabel parts) (LabelMatch mlabel binds, body) =
			just_if (mlabel == slabel) $ interpret' bt s' body
			where s' = zip (map fst binds) parts ++ newscope
		
		match _ _ = Nothing


-----------

primOps :: [(String, (String -> String) -> [Literal] -> Literal)]
primOps = map_assoc (binPrimOp unwrapInt IntLiteral) [
			("GHC.Prim.+#", (+)),
			("GHC.Prim.-#", (-)),
			("GHC.Prim.*#", (*)),
						
			("+", (+)),
			("-", (-)),
			("*", (*)),
			("/", quot)
		  ] ++ map_assoc (binPrimOp unwrapInt wrapBool) [
			("GHC.Prim.<#", (<)),
			("GHC.Prim.<=#", (<=)),
			
			("GHC.Prim.>#", (>)),
			("GHC.Prim.>=#", (<=)),
			
			("GHC.Prim.==#", (==)),
			("GHC.Prim./=#", (/=))
		  ] ++ map_assoc (binPrimOp unwrapBool wrapBool) [
			("Data.Bool.&&", (&&)),
			("Data.Bool.||", (||))
    	  ]
	where	
	unwrapInt :: (String -> String) -> Literal -> Integer
	unwrapInt _ (IntLiteral i) = i
	unwrapInt failmsg lit = error . failmsg $ "not an int: " ++ (showSDoc $ pprLiteral lit)
	
	unwrapBool :: (String -> String) -> Literal -> Bool
	unwrapBool _ (SumLiteral "GHC.Types.Bool" "GHC.Types.True" []) = True
	unwrapBool _ (SumLiteral "GHC.Types.Bool" "GHC.Types.False" []) = False
	unwrapBool failmsg lit = error . failmsg $ "not a bool: " ++ (showSDoc $ pprLiteral lit)
	
	wrapBool True = SumLiteral "GHC.Types.Bool" "GHC.Types.True" []
	wrapBool False = SumLiteral "GHC.Types.Bool" "GHC.Types.False" []

	binPrimOp :: ((String -> String) -> Literal -> a) -> (b -> Literal)
		 	  -> (a -> a -> b)
		 	  -> (String -> String) -> [Literal]
			  -> Literal
	binPrimOp unwrap wrap op failmsg [lhs, rhs] =
		wrap $ op (unwrap failmsg lhs) (unwrap failmsg rhs)
	binPrimOp _ _ _ failmsg _ = error $ failmsg "wrong number of arguments"




