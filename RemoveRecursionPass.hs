
module RemoveRecursionPass
where

import IR
import IRUtils
import IRTransforms
import CombinationPass

import Data.List as List
import Data.Tuple

------------------------------
{-
NOTE: this assumes all variable names are unique/not shadowing

 let [a = recurse(), b = k, c = recurse()] e
  -> let [a = recurse()] let [c = recurse()] let [b = k] e
  
  notes:
  - there is actually a scheduling issue here: where to put the non-recursive
	bindings among the recursive ones.  After other transforms to make the 
	function tail recursive, the placement of the non-recursive-calling bindings
	will determine on what cycle they're computed.  Here, this is punted on,
	and I place them after all recursions in the Let.  The reasoning is that it
	means that they will not have to be passed around as captures
-}

isolate_recursions :: String -> Expr -> Expr
isolate_recursions fname = map_expr (\x -> case x of
	Let bindings cont ->
		let (recursives, nonrecursives) = partition isRecursive bindings
		    cont' = Let nonrecursives cont
		in foldl (.) id (map (\b -> Let [b]) recursives) cont'
	_ -> x)
	where
	isRecursive :: (String, Expr) -> Bool
	isRecursive (_, expr) = expr_recursiveness fname expr /= Nonrecursive

------------------------------
{-
 assuming extract_subexprs and flatten_lets
-}

return_through_continuation :: String -> String -> Expr -> Expr -> Expr
return_through_continuation outerfname fcname cpv = 
  map_return (\expr -> case expr of
    Call rt fname args | fname == outerfname -> Call rt fname (args ++ [cpv])
    _ -> Call (typeOfExpression expr) fcname [expr, cpv]
  )
  
------------------------------

separate_recursions :: String -> Type -> Expr -> Type -> Expr
                     -> (
                          [((Label, [Type]), (Pattern, Expr))],
                          [(String, Function)],
                          Expr
                        )
separate_recursions fname rt returnedValue fcontsumtype =
    (\((_, cres, fcres), e) -> (cres, fcres, e)) . separate_recursions' 0
    where 
    separate_recursions' :: Integer -> Expr
                     -> (
                          (
                            Integer,
                            [((Label, [Type]), (Pattern, Expr))],
                            [(String, Function)]
                          ),
                          Expr
                        )
    separate_recursions' startcnum = 
        map_accum_expr' (\accum orig expr -> case expr of
            Let [(rname, Call _ f rargs)] continuation | f == fname ->
                let (cnum, cresults, fresults) = accum
                
                    captures' = get_captures continuation
                    captures = filter (\(_, vname) -> vname /= rname) captures'
                    
                    label = "L" ++ fname ++ "$C" ++ (show cnum)
                    copt = (label, map fst captures)

                    continuationArg = Call fcontsumtype label $ map (uncurry Var) captures
                    newexpr = Let [("$ca", continuationArg)] newcall
                    newcall = Call rt fname (rargs ++ [(Var fcontsumtype "$ca")])
                    
                    cbranch = (LabelMatch label (map swap captures),
                                 Let [(rname, returnedValue)] continuation)
                    result = (copt, cbranch) 
                in ((cnum + 1, result : cresults, fresults), newexpr)
                
            Let [(_, c@(Case _ _ _))] _ | expr_recursiveness fname c /= Nonrecursive ->
                let (oaccum, oexpr) = orig
                    (cnum, cresults, fresults) = oaccum
                    Let [(bname, Case casem casev branches')] continuation = oexpr
                
                    captures' = get_captures continuation
                    captures = filter (\(_, vname) -> vname /= bname) captures'

                    bTy = typeOfExpression c
                    -- TODO: if bTy == rt, do a cresult, not an fresult
                     
                    cfname = "$CF" ++ show cnum ++ "_" ++ fname
                    cfn = Function bTy ((bTy, bname) : captures) continuation
                    result = (cfname, cfn)
                                        
                    branches = map_assoc (map_return cfforbranch) branches'
                    cfforbranch e = Let [("res", e)] cfcall
                    cfcall = Call rt cfname $ map (uncurry Var) ((bTy, "res") : captures)

                    newexpr' = Case casem casev branches
                    (res', newexpr) = separate_recursions' (cnum + 1) newexpr'
                    (cnum', cres', fres') = res'
                in ((cnum', cres' ++ cresults, result : fresults ++ fres'), newexpr)
            
            _ -> (accum, expr)
        ) (startcnum, [], [])

------------------------------

tail_recursive :: String -> Function -> Namespace
tail_recursive fname (Function rt args fImpl) = newns
    where
    cArgTypeName = "CT" ++ fname
    fcname = fname ++ "$C"
    cArgName = "$c"
    frname = fname ++ "$R"
    rtargname = "$rtarg"
    cfargname = "$cfarg"
    bottomOfStackLabel = "L" ++ fname ++ "$bos"
    combinedFname = fname ++ "$TR"
    
    cArgTypeRef = TypeRef cArgTypeName
    rtargvar = Var rt rtargname
    
    impl2 = rename_function fname frname $
            remove_let_indirection $
            remove_renaming $
            remove_unused_variables $
            isolate_recursions fname $
            flatten_lets $
            extract_subexprs $
            unique_namings (map snd args) $
            fImpl
        
    impl3 = return_through_continuation frname fcname (Var cArgTypeRef cArgName) impl2
    (conts, fconts, impl4) = separate_recursions frname rt rtargvar cArgTypeRef impl3
    fr = Function rt (args ++ [(cArgTypeRef, cArgName)]) impl4

    (cArgTypeOpts, cfbranches) = unzip conts    
    cArgType = SumType $ (bottomOfStackLabel, []) : cArgTypeOpts
    returnBranch = (LabelMatch bottomOfStackLabel [], rtargvar)        
    cfimpl = Case (Var cArgTypeRef cfargname) "_" (returnBranch : cfbranches)
    fc = Function rt [(rt, rtargname), (cArgTypeRef, cfargname)] cfimpl

    fsToCombine = [(frname, fr), (fcname, fc)] ++ fconts
    (combinedFns, combinationTys) = combine_functions' combinedFname rt fsToCombine
    combinedFunction = assume $ List.lookup combinedFname combinedFns
    rWrapper = assume $ List.lookup frname combinedFns
    
    -- you could probably do this much more directly
    bosLiteral = Var cArgTypeRef bottomOfStackLabel
    wrapperf = Function rt args $ inline_functions [(frname, rWrapper)] $
        Call rt frname ((map (uncurry Var) args) ++ [bosLiteral])
        
    newfns = [(combinedFname, combinedFunction), (fname, wrapperf)]
    newtys = (cArgTypeName, cArgType) : combinationTys
    newns = (newfns, newtys)
    
------------------------------

tail_recursion_pass :: Module -> Module
tail_recursion_pass m = m { mnamespace = clean $ pass (mnamespace m) }
	where
	clean :: Namespace -> Namespace
	clean = transform_namespace cleaners
	cleaners :: Expr -> Expr
	cleaners = remove_let_indirection . remove_renaming
	
	pass :: Namespace -> Namespace
	pass (fns, tys) = foldl combine_namespaces ([], tys) $ map perform fns
	
	perform :: (String, Function) -> Namespace
	perform (fname, fn) 
		| expr_recursiveness fname (funcImpl fn) == Recursive = 
			tail_recursive fname fn
	perform f = ([f], [])

------------------------------

module_recursiveness_stats :: Module -> String
module_recursiveness_stats (Module _ (fns, _)) =
    intercalate "\n" $ (show worstRecursiveness) : map show allstats
    where
    worstRecursiveness = maximum $ map (\(_, r, _) -> r) allstats
    allstats = map stats fns
    stats (fname, fn) = (fname, recursiveness, deps)
        where expr = funcImpl fn
              recursiveness = expr_recursiveness fname expr
              deps = function_dependencies expr



