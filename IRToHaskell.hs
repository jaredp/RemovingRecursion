
module IRToHaskell where

import Outputable
import IR
import IRUtils
import IRTransforms

import Data.List ( sort )

moduleToHaskell :: Module -> String
moduleToHaskell m = showSDoc $ pprModule (transform_all_functions unrecbindings m)

showLiteral :: Literal -> String
showLiteral = showSDoc . pprLiteral

unrecbindings :: Function -> Function
unrecbindings (Function rt args impl) = 
    Function rt args $ unique_namings (map snd args) impl
    
-- http://www.haskell.org/ghc/docs/latest/html/libraries/ghc/Outputable.html

pprModule :: Module -> SDoc
pprModule (Module name (fns, tys)) =
    text "module" <+> textId name <+> text "where" $$
    text "import GHC.Types" $$
    text "import GHC.Prim" $$
    text "import GHC.Base" $$
    pprNamespace ns
    where
    ns = (remove_ghc_builtins fns, remove_ghc_builtins tys)
    remove_ghc_builtins = filter (not . startswith "GHC." . fst)
    startswith prefix str = take (length prefix) str == prefix

showNamespace :: Namespace -> String
showNamespace = showSDoc . pprNamespace

pprNamespace :: Namespace -> SDoc
pprNamespace (fns, tys) =
      vcat (map (\vd -> blankLine $$ pprBoundType vd) tys) $$
      vcat (map (\vd -> blankLine $$ pprBoundFunction vd) fns)
      
pprBoundFunction :: (String, Function) -> SDoc
pprBoundFunction (fname, Function rt args impl) = 
      textId fname <+> text "::" <+> sep fsig $$
      (hang (textId fname <+> (hsep $ map (textId . snd) args) <+> equals) 2 $
      pprExpr impl)
      where
      --FIXME: punctuate
      fsig = map_rs (arrow<+>) $ map pprType (map fst args ++ [rt])
      map_rs _ [] = []
      map_rs fn (x : xs) = x : (map fn xs)

pprBoundType :: (String, SumType) -> SDoc
pprBoundType (tname, t) = text "data" <+> textId tname <+> ptype t -- deriving (Show)?
	where
    ptype (SumType opts) = sep $ pOpts
        where
        --FIXME: punctuate 
        pOpts = map_tail (equals <+>) (char '|' <+>) $ map pOpt opts
        map_tail _ _ [] = []
        map_tail f m (x : xs) = f x : (map m xs)
        pOpt (label, components) = textId label <+> (hsep $ map pprType components)

pprType :: Type -> SDoc
pprType IntType = text "Int"
pprType (TypeRef t) = textId t

pprExpr :: Expr -> SDoc
pprExpr (Var _ v) = textId v
pprExpr (Literal l) = pprLiteral l
pprExpr (Call _ u el) = parens $ sep (parens (textId u) : (map pprExpr el))
pprExpr (Let defs e) =
      text "let" <+>
      vcat (map (\(vb, ex) -> textId vb <+> equals <+> pprExpr ex) defs)
      <+> text "in" $$
      pprExpr e
      
pprExpr (Case m v [(LabelMatch l bindings, c)]) =
      prebinding $$
      text "let" <+> binding <+> equals <+> inspected <+> text "in" $$
      pprExpr c
      where
      binding = hsep $ map textId (l : map fst bindings)
      prebinding = if v == "_"
        then empty
        else text "let" <+> textId v <+> equals <+> pprExpr m <+> text "in"
      inspected = if v == "_"
         then pprExpr m
         else textId v
         
pprExpr (Case m v alts') =
      hang control 3
        (vcat $ map (\(p, b) -> hang (pprPattern p <+> text "->") 3 (pprExpr b)) alts)
      where
      alts = sort alts'
      control =
        if v == "_"
        then text "case" <+> pprExpr m <+> text "of"
        else text "let" <+> textId v <+> equals <+> pprExpr m 
             <+> text "in case" <+>  textId v <+> text "of"

pprPattern :: Pattern -> SDoc
pprPattern DefaultMatch = text "_"
pprPattern (LiteralMatch l) = pprLiteral l	
pprPattern (LabelMatch label bindings) = hsep $ map textId (label : map fst bindings)

pprLiteral :: Literal -> SDoc
pprLiteral (IntLiteral i) = integer i
pprLiteral (SumLiteral _ label []) = textId label
pprLiteral (SumLiteral _ label c) = parens $ sep (textId label : map pprLiteral c)

textId :: String -> SDoc
textId = text . mangleId

mangleId :: String -> String
mangleId [] = []
mangleId (x:xs) = substitute x ++ mangleId xs
  where
  substitute '_' = "__"
  substitute '$' = "_S"
  substitute '|' = "_l"
  substitute c = [c]

