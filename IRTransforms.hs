
module IRTransforms where

import IR
import IRUtils

import Data.List as List
import Data.Maybe


{-

TODO:
reschedule_lets
flatten constant exprs

-}

------------------------------

remove_case_indirection :: Expr -> Expr
remove_case_indirection = map_expr (\expr -> case expr of
    Case _ "_" [(DefaultMatch, c)] -> c
    Case m v [(DefaultMatch, c)] -> Let [(v, m)] c
    _ -> expr
  )

------------------------------
{-
-- assumes unique variable names

inline_var :: String -> Expr -> Expr -> Expr
inline_var vname replacement = map_expr (\expr -> case expr of
    Var _ vname' | vname' == vname -> replacement
    _ -> expr
  )
-}
------------------------------

getpbinds :: Pattern -> [String]
getpbinds (LabelMatch _ binds) = map fst binds
getpbinds _ = []

--Only first-level!!
get_binds :: Expr -> [String]
get_binds (Let bindings _) = map fst bindings
get_binds (Case _ v brs) = v : concatMap (getpbinds . fst) brs
get_binds _ = []

------------------------------

rename_variable :: String -> String -> Expr -> Expr
rename_variable oldvar newvar = map_expr' (\old expr -> case (old, expr) of
        (_, Var t vname) | vname == oldvar -> Var t newvar
        (Let _ oldc, Let newbs _) | oldvar `elem` get_binds old -> Let newbs oldc
        (Case _ _ oldbrs, Case newm v newbrs) | oldvar `elem` get_binds old ->
            Case newm v $ map (\(oldbr@(oldp, _), newbr) ->
                if oldvar `elem` getpbinds oldp
                then oldbr
                else newbr
            ) $ zip oldbrs newbrs
        _ -> expr)

------------------------------

rename_function :: String -> String -> Expr -> Expr
rename_function oldfname newfname = map_expr (\expr -> case expr of
	Call rt fname args | fname == oldfname -> Call rt newfname args
	_ -> expr)

------------------------------

extract_subexprs :: Expr -> Expr
extract_subexprs = snd . map_accum_expr (\tcount expr -> case expr of
    Case m v branches ->
        let (v', tcount') = if v == "_" then next_t tcount else (v, tcount)
            mvar = Var (typeOfExpression m) v'
        in (tcount', Let [(v', m)] $ Case mvar "_" branches)
    Call rt fname args ->
        let (tcount', extractedargs) = mapAccumL (\t arg -> 
                let (newname, t') = next_t t
                    argty = typeOfExpression arg
                in (t', ((newname, arg), Var argty newname))
              ) tcount args
            (bindings, argvars) = unzip extractedargs
        in (tcount', Let bindings $ Call rt fname argvars)
    _ -> (tcount, expr)
  ) 0
  where
  next_t :: Integer -> (String, Integer)
  next_t t = ("$t" ++ show t, t + 1)

------------------------------

--FIXME: there's a much cleaner way to do the following two
append_to_let_chain :: String -> Expr -> Expr -> Expr
append_to_let_chain crn (Let chainLinkBindings cont) exprToAppend =
    Let chainLinkBindings $ append_to_let_chain crn cont exprToAppend
    
append_to_let_chain chainResultNewname chainTail exprToAppend =
    Let [(chainResultNewname, chainTail)] exprToAppend

-- doesn't do variable renaming -- call uniquify_variables first

flatten_lets :: Expr -> Expr
flatten_lets = map_expr (\expr -> case expr of
    Let bindings continuation -> uncurry Let $
      List.foldl (\(newbindings, newcontinuation) binding@(bname, bexpr) -> 
        case bexpr of
          innerlet@(Let _ _) -> 
               (newbindings, append_to_let_chain bname innerlet newcontinuation)
          _ -> (binding : newbindings, newcontinuation)
      ) ([], continuation) bindings
    _ -> expr)

------------------------------

-- this doesnt handle Let ["a" = e] (Let ["f" = k] (Var "a")) I think... should it?

-- Let ["a" = e] (Var "a") -> e
-- Let [] e -> e

remove_let_indirection :: Expr -> Expr
remove_let_indirection = map_expr (\expr -> case expr of
	Let bindings (Var _ vref) -> fromMaybe expr (List.lookup vref bindings)
	Let [] cont -> cont
	_ -> expr)

------------------------------

remove_renaming :: Expr -> Expr
remove_renaming = map_expr (\expr -> case expr of
    Let bindings cont ->
        let 
        (b', newcont) = List.foldl (\(newbindings, e) b@(name, binding) ->
            case binding of
                Var _ oldname -> (newbindings, rename_variable name oldname e)
                _ -> (b:newbindings, e)
            ) ([], cont) bindings
        in Let b' newcont
    _ -> expr)

------------------------------

inline_functions :: [(String, Function)] -> Expr -> Expr
inline_functions functions = map_expr (\expr -> case expr of
    Call _ fname callArgs ->
        let fn' = List.lookup fname functions in
        case fn' of 
            Nothing -> expr
            Just(fn) ->
                Let (zip (map snd $ funcArgs fn) callArgs) (funcImpl fn)
    _ -> expr)

------------------------------

remove_unused_variables :: Expr -> Expr
remove_unused_variables = map_expr act
	where 
	act (Let bindings body) = Let newbindings body
		where
		usedvars = map snd $ get_captures body
		newbindings = filter (\x -> fst x `elem` usedvars) bindings 
	act x = x

------------------------------

unique_namings :: [String] -> Expr -> Expr
unique_namings o e = snd $ map_accum_expr (\defined expr -> case expr of
        Let bindings cont ->
            let ((defs', c'), newbs) = mapAccumL (\(existing, c) (v, bound) ->
                    let (v', e') = rename v existing in
                    ((e', rename_variable v v' c), (v', bound))
                    ) (defined, cont) bindings
            in (defs', Let newbs c')
            
        Case match v branches1 ->
            let (defs5, branches2) = mapAccumL (\defs1 (p, bc1) -> case p of
                    LabelMatch label binds1 ->
                        let ((defs4, bc3), binds2) = mapAccumL (\(defs2, bc2) (vi, t) ->
                                let (v', defs3) = rename vi defs2 in
                                ((defs3, rename_variable vi v' bc2), (v', t))
                                ) (defs1, bc1) binds1
                        in (defs4, (LabelMatch label binds2, bc3))
                    _ -> (defs1, (p, bc1))
                    ) defined branches1
            in let (v', defs6) = rename v defs5 in
            (defs6, Case match v' $ map_assoc (rename_variable v v') branches2)
        
        _ -> (defined, expr)
    ) o e
    where
    rename i existing = (i', i' : existing)
      where
      i' = first i 1
      first p n = if p `notElem` existing then p else first (indexed n) (n + 1)
      indexed :: Int -> String
      indexed n = i ++ show n

