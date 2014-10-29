
module CombinationPass
where

import IR
import IRUtils
import IRTransforms

import Data.Maybe
import Data.List ( (\\), intercalate )
import Data.Either
import Data.Graph
import Data.Tuple

------------------------------

{-

FIXME: Currently assumes all functions have the same return type. This isn't a
problem for the multiple returns -> tail recursion transform, but otherwise is
relevant.  Actually, that transform raises the question of what optimizations
would/could be given for sum types with only one wrapped type.
FIXME: combine_functions should return a SumType of the different retvals 
of its component functions.

-}

combine_functions' :: String -> Type -> [(String, Function)] -> Namespace
combine_functions' combinedFname rt fns = (combinedFn : wrappers, tys)
    where
    labelForFn fname = "T$L_" ++ fname
    dataOptForFn (fname, Function _ args _) = (labelForFn fname, map fst args)
    combinedArgsType = SumType (map dataOptForFn fns)
     
    argstypeName = "ArgTy$" ++ combinedFname
    tys = [(argstypeName, combinedArgsType)]
    argsTypeRef = TypeRef argstypeName
        
    argsname = "$arg"
    newarg = [(argsTypeRef, argsname)]
    
    branchForFn (fname, Function _ args impl) = (pattern, impl) where
        bindings = map swap args
        pattern = LabelMatch (labelForFn fname) bindings        
    branches = map branchForFn fns
    
    control = Case (Var argsTypeRef argsname) "_" branches
    combinedImpl = inline_functions wrappers control
    combinedFn = (combinedFname, Function rt newarg combinedImpl)

    wrapperForFn (fname, fn) = wrapper where
        argsVars = map (uncurry Var) (funcArgs fn)
        uncurrying_call = Call argsTypeRef (labelForFn fname) argsVars
        callToCombined = Call rt combinedFname [uncurrying_call]
        wrapper = fn { funcImpl = callToCombined }
    wrappers = map (lift_assoc wrapperForFn) fns

------------------------------

combine_functions :: [(String, Function)] -> Namespace
combine_functions fns = combine_functions' newfname rt fns
    where
    -- wrap and unwrap return types here
    rt = funcReturnType . snd . head $ fns
    newfname = "$combined_" ++ (intercalate "|" (map fst fns))

------------------------------

combination_pass :: Module -> [(String, Expr)] -> Module
combination_pass (Module name startNS@(funDefs, _)) varRRExprs = 
  Module name (clean_namespace ns)
  where
  -- Convert top-level variable definitions: constant, so convert them
  -- to literals.  Also, RRir globals are Literals.

  getCaptureNames :: Expr -> [String]
  getCaptureNames = map snd . get_captures
  
  getExprDeps (_, e) = getCaptureNames e ++ function_dependencies e
  getFnDeps (fname, Function _ args impl) = free_variables ++ fn_deps
	where
    free_variables = getCaptureNames impl \\ map snd args
    fn_deps = function_dependencies impl \\ [fname]
  key_and_deps nmap getDeps (key, val) = (nmap (key, val), key, getDeps (key, val))
  
  varDeps = map (key_and_deps Left getExprDeps) varRRExprs
  funDeps = map (key_and_deps Right getFnDeps) funDefs
  topLevelDeps = varDeps ++ funDeps

  -- the fn defs will change by the time we use them 
  recursiveFnSCCs :: [[String]]
  (orderedVarExprs, recursiveFnSCCs) = partitionEithers $ mapMaybe (\c ->
      case c of
        AcyclicSCC (Left var) -> Just (Left var)
        AcyclicSCC (Right _) -> Nothing
        CyclicSCC thescc ->
            let (vars, fns) = partitionEithers thescc in
            if vars /= [] then error $ "scc has vars: " ++ show thescc else
            Just (Right $ map fst fns)
    ) (stronglyConnComp topLevelDeps)
  
  inliner = foldl (\inliner' gbl -> inliner' . Let [gbl]) id orderedVarExprs
  deGHC = remove_let_indirection .
          remove_unused_variables .
          remove_renaming .
          remove_case_indirection .
          inliner
  ns'@(fns', _) = transform_namespace deGHC startNS
    
  getFn fname = assume $ lookup fname fns'
  mutuallyRecFnGroups = map (associate getFn) recursiveFnSCCs
  mutualRecursionFreeNamespaces = map combine_functions mutuallyRecFnGroups
  ns = foldl combine_namespaces ns' mutualRecursionFreeNamespaces 
  

