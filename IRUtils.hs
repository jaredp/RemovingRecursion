module IRUtils where

import IR
import Data.List as List
import Data.Tuple as Tuple
import qualified Data.Set as Set

-- general utils

assume :: Maybe a -> a
assume = assume_ "incorrect assumption"

assume_ :: String -> Maybe a -> a
assume_ _ (Just x) = x
assume_ e Nothing = error e

first_found :: [Maybe a] -> a 
first_found (Nothing : xs) = first_found xs
first_found (Just x : _) = x
first_found [] = error "something not found"

maybe_apply :: Maybe a -> (a -> b) -> Maybe b
maybe_apply (Just x) op = Just (op x)
maybe_apply Nothing _ = Nothing

just_if :: Bool -> a -> Maybe a
just_if cond x = if cond then Just x else Nothing

associate :: (a -> b) -> [a] -> [(a, b)]
associate map_fn l = map (\e -> (e, map_fn e)) l

lift_assoc :: ((a, b) -> c) -> (a, b) -> (a, c)
lift_assoc x e = (fst e, x e)

map_assoc :: (a -> b) -> [(c, a)] -> [(c, b)]
map_assoc map_fn l = map (\(f, s) -> (f, map_fn s)) l

map_fst :: (a -> b) -> [(a, c)] -> [(b, c)]
map_fst map_fn l = map (\(f, s) -> (map_fn f, s)) l

mapAccumLSnd :: ([b] -> (d, [c])) -> [(a, b)] -> (d, [(a, c)])
mapAccumLSnd fn l = (accum, zip f mapped)
    where
    (f, s) = unzip l
    (accum, mapped) = fn s

------------------------------

transform_function :: (Expr -> Expr) -> Function -> Function
transform_function t f = f { funcImpl = t (funcImpl f) }

------------------------------

transform_all_functions :: (Function -> Function) -> Module -> Module
transform_all_functions t (Module name (fns, tys)) =
    Module name (map_assoc t fns, tys)

------------------------------

transform_namespace :: (Expr -> Expr) -> Namespace -> Namespace
transform_namespace t (fns, tys) = (map_assoc (transform_function t) fns, tys)

------------------------------

map_expr :: (Expr -> Expr) -> Expr -> Expr
map_expr transform (Call rt fname args) = transform newcall
	where
	mapper = map_expr transform
	newcall = Call rt fname (map mapper args)

map_expr transform (Case match v branches) = transform newcase
	where
	mapper = map_expr transform
	newmatch = mapper match
	newbranches = map_assoc mapper branches
	newcase = Case newmatch v newbranches
	
map_expr transform (Let bindings continuation) = transform newlet
	where
	mapper = map_expr transform 
	newbindings = map_assoc mapper bindings
	newcontinuation = mapper continuation
	newlet = Let newbindings newcontinuation
	
map_expr transform x = transform x

------------------------------

-- by using map_accum_expr' and the (accum, orig) arg, 
-- you can do postorder traversal sometimes

map_accum_expr' :: (a -> (a, Expr) -> Expr -> (a, Expr)) -> a -> Expr -> (a, Expr)
map_accum_expr' transform accum orig@(Call rt fname args) =
    transform accum' (accum, orig) newcall
    where
    mapper = map_accum_expr' transform
    (accum', newargs) = List.mapAccumL mapper accum args
    newcall = Call rt fname newargs
    
map_accum_expr' transform accum orig@(Case match v branches) =
    transform accum'' (accum, orig) newcase
    where
    mapper = map_accum_expr' transform
    (accum', newbranches) = mapAccumLSnd (List.mapAccumL mapper accum) branches
    (accum'', newmatch) = mapper accum' match
    newcase = Case newmatch v newbranches

map_accum_expr' transform accum orig@(Let bindings continuation) =
    transform accum'' (accum, orig) newlet
    where
    mapper = map_accum_expr' transform
    (accum', newbindings) = mapAccumLSnd (List.mapAccumL mapper accum) bindings
    (accum'', newcontinuation) = mapper accum' continuation
    newlet = Let newbindings newcontinuation
    
map_accum_expr' transform accum x = transform accum (accum, x) x

map_accum_expr :: (a -> Expr -> (a, Expr)) -> a -> Expr -> (a, Expr)
map_accum_expr trans = map_accum_expr' (\a _ e -> trans a e)

------------------------------

map_expr' :: (Expr -> Expr -> Expr) -> Expr -> Expr
map_expr' t = snd . map_accum_expr' (\_ (_, o) e -> ((), t o e)) ()

------------------------------

map_return :: (Expr -> Expr) -> Expr -> Expr
map_return m (Let bindings cont) =
	Let bindings (map_return m cont)

map_return m (Case match v branches) = 
	Case match v $ (map_assoc $ map_return m) branches
	
map_return m x = m x

------------------------------

typeOfExpression :: Expr -> Type
typeOfExpression (Call t _ _) = t
typeOfExpression (Let _ c) = typeOfExpression c
typeOfExpression (Var t _) = t
typeOfExpression (Literal l) = typeOfLiteral l
typeOfExpression (Case _ _ ((_, e):_)) = typeOfExpression e
typeOfExpression (Case m _ []) =
	error $ "case on \"" ++ show m ++ "\" with no branches"

typeOfLiteral :: Literal -> Type
typeOfLiteral (IntLiteral _) = IntType
typeOfLiteral (SumLiteral t _ _) = TypeRef t

------------------------------

get_captures :: Expr -> [(Type, String)]
get_captures = Set.toList . get_captures'

get_captures' :: Expr -> Set.Set (Type, String)
get_captures' (Literal _) = Set.empty
get_captures' (Var t name) = Set.singleton (t, name)
get_captures' (Call _ _ args) = Set.unions $ map get_captures' args	

get_captures' (Case matching v cases) =
	matching_caps `Set.union` (Set.delete (type_of_matched, v) all_branch_captures)
	where
	matching_caps = get_captures' matching
	type_of_matched = typeOfExpression matching
	all_branch_captures = Set.unions $ map branch_captures cases
	branch_captures (pattern, expr) = 
		get_captures' expr `Set.difference` (get_pbindings pattern)
		where
		get_pbindings (LabelMatch _ bnds) = Set.fromList $ map Tuple.swap bnds
		get_pbindings _ = Set.empty

get_captures' (Let bindings continuation) = 
	binding_caps `Set.union` (cont_caps `Set.difference` let_binds)
	where
	cont_caps = get_captures' continuation
	binding_caps = Set.unions $ map (get_captures' . snd) bindings
	let_binds = Set.fromList $ map Tuple.swap $ map_assoc typeOfExpression bindings

------------------------------

--use Set?
function_dependencies' :: Expr -> [String]

function_dependencies' (Call _ fn args) =
	fn : (List.concat $ map function_dependencies' args)

function_dependencies' (Case matching _ branches) =
	function_dependencies' matching ++
	(List.concat $ map (function_dependencies' . snd) branches)

function_dependencies' (Let bindings continuation) =
	(List.concat $ map (function_dependencies' . snd) bindings)
	++ function_dependencies' continuation
	
function_dependencies' _ = []	-- Vars, Literals

function_dependencies :: Expr -> [String]
function_dependencies e = List.nub $ function_dependencies' e

------------------------------

data Recursiveness = Nonrecursive | TailRecursive | Recursive
				   deriving (Show, Eq, Ord)

nontail :: Recursiveness -> Recursiveness
nontail Nonrecursive = Nonrecursive
nontail _ = Recursive

expr_recursiveness :: String -> Expr -> Recursiveness
expr_recursiveness fname (Call _ fcall args) =
	maximum (call_recursiveness : args_recursiveness)
	where
	call_recursiveness = if fcall == fname then TailRecursive else Nonrecursive
	args_recursiveness = map (nontail . (expr_recursiveness fname)) args
	
expr_recursiveness fname (Case matching _ patterns) =
	maximum (match_recursiveness : branch_recursiveness)
	where
	match_recursiveness = nontail $ expr_recursiveness fname matching
	branch_recursiveness = map ((expr_recursiveness fname) . snd) patterns
	
expr_recursiveness fname (Let bindings continuation) =
	maximum (c_recursiveness : bindings_recursiveness)
	where
	c_recursiveness = expr_recursiveness fname continuation
	bindings_recursiveness = map (nontail . expr_recursiveness fname . snd) bindings
	
expr_recursiveness _ _ = Nonrecursive	-- Vars and Literals

------------------------------

combine_namespaces :: Namespace -> Namespace -> Namespace
combine_namespaces (fs, ts) (fs', ts') = (fs' ++ fs, ts' ++ ts)

------------------------------

clean_namespace :: Namespace -> Namespace
clean_namespace (fns, tys) = (remove_overridden fns, remove_overridden tys)
	where remove_overridden = nubBy (\x y -> fst x == fst y)

------------------------------

ns_add_type :: (String, SumType) -> Namespace -> Namespace
ns_add_type t (fns, tys) = (fns, t : tys)

------------------------------

get_all_fnames :: Namespace -> [String]
get_all_fnames (fns, tys) = map fst fns ++ concatMap get_ctors tys

------------------------------

get_all_ctors :: Namespace -> [String]
get_all_ctors (_, tys) = concatMap get_ctors tys

------------------------------

get_ctors :: (String, SumType) -> [String]
get_ctors (_, (SumType opts)) = map fst opts

------------------------------



