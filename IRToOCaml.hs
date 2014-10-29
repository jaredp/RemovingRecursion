
module IRToOCaml where

import Outputable
import IR
import IRUtils
import IRTransforms

import Data.List ( sort, intersperse )
import Data.Char ( isAsciiUpper )

moduleToOCaml :: Module -> String
moduleToOCaml m = showSDoc $ pprModule (transform_all_functions unrecbindings m)

showLiteral :: Literal -> String
showLiteral = showSDoc . pprLiteral

unrecbindings :: Function -> Function
unrecbindings (Function rt args impl) = 
    Function rt args $ unique_namings (map snd args) impl
    
-- http://www.haskell.org/ghc/docs/latest/html/libraries/ghc/Outputable.html

map_tail :: (a -> a) -> (a -> a) -> [a] -> [a]
map_tail _ _ [] = []
map_tail f m (x : xs) = f x : (map m xs)

comma_separated :: [SDoc] -> SDoc
comma_separated l = hcat $ intersperse (comma <> space) l

---------

pprModule :: Module -> SDoc
pprModule (Module _ (fns, tys)) =
    pprNamespace ns
    where
    ns = (remove_ghc_builtins fns, remove_ghc_builtins tys)
    remove_ghc_builtins = filter (not . startswith "GHC." . fst)
    startswith prefix str = take (length prefix) str == prefix

showNamespace :: Namespace -> String
showNamespace = showSDoc . pprNamespace

pprNamespace :: Namespace -> SDoc
pprNamespace (fns, tys) =
      (vcat $ map_tail (text "type" <+>) (blankLine $$ text "and" <+>)
        (map pprBoundType tys))
      $$ blankLine $$
      (vcat $ map_tail (text "let rec" <+>) (blankLine $$ text "and" <+>)
        (map pprBoundFunction fns))
                
pprBoundFunction :: (String, Function) -> SDoc
pprBoundFunction (fname, Function _ args impl) = 
      hang (textId fname <+> (hsep $ map (textId . snd) args) <+> equals) 2 
      $ pprExpr impl

pprBoundType :: (String, SumType) -> SDoc
pprBoundType (tname, SumType opts) = typeId tname <+>
    (sep $ map_tail (equals <+>) (char '|' <+>) $ map pOpt opts)
    where
    pOpt :: (String, [Type]) -> SDoc
    pOpt (label, []) = textId label
    pOpt (label, components) =
        textId label <+> text "of" <+>
        sep (intersperse (char '*') $ map pprType components)

pprType :: Type -> SDoc
pprType IntType = text "int"
pprType (TypeRef t) = typeId t

pprExpr :: Expr -> SDoc
pprExpr (Var _ v) = textId v
pprExpr (Literal l) = pprLiteral l
pprExpr (Call _ ctor args) | isAsciiUpper (head ctor) =
    parens (textId ctor <+> parens (comma_separated $ map pprExpr args))

pprExpr (Call _ fname el) = 
    parens $ sep (parens (space <> textId fname <> space) : (map pprExpr el))

pprExpr (Let defs e) = (vcat $ map binding defs) $$ pprExpr e
    where
    binding :: (String, Expr) -> SDoc
    binding (v, expr) = text "let" <+> textId v <+> equals <+> pprExpr expr <+> text "in"

pprExpr (Case m "_" [(LabelMatch l bindings, c)]) =
    text "let" <+> patternMatch l bindings <+> equals <+> pprExpr m <+>
    text "in" $$ pprExpr c
      
pprExpr (Case m v [(LabelMatch l bindings, c)]) =
    text "let" <+> textId v <+> equals <+> pprExpr m <+> text "in" $$
    text "let" <+> patternMatch l bindings <+> equals <+> textId v <+>
    text "in" $$ pprExpr c
         
pprExpr (Case m v alts') =
      parens (control $$ (vcat $ map_tail (space <+>) (char '|' <+>) $ map branch alts))
      where
      alts = sort alts'
      branch (p, b) = hang (pprPattern p <+> text "->") 3 (pprExpr b)
      control =
        if v == "_"
        then text "match" <+> pprExpr m <+> text "with"
        else text "let" <+> textId v <+> equals <+> pprExpr m 
             <+> text "in match" <+> textId v <+> text "with"

patternMatch :: String -> [(String, Type)] -> SDoc
patternMatch l [] = textId l
patternMatch l bindings = 
    textId l <+> parens (comma_separated $ map (textId . fst) bindings)

pprPattern :: Pattern -> SDoc
pprPattern DefaultMatch = text "_"
pprPattern (LiteralMatch l) = pprLiteral l	
pprPattern (LabelMatch label bindings) = patternMatch label bindings

pprLiteral :: Literal -> SDoc
pprLiteral (IntLiteral i) = integer i
pprLiteral (SumLiteral _ label []) = textId label
pprLiteral (SumLiteral _ label c) = parens $ sep (textId label : map pprLiteral c)

textId :: String -> SDoc
textId = text . mangleId

typeId :: String -> SDoc
typeId = text . mangleId . ("t" ++)

mangleId :: String -> String
mangleId [] = []
mangleId (x:xs) = substitute x ++ mangleId xs
  where
  substitute '_' = "__"
  substitute '$' = "_S"
  substitute '|' = "_l"
  substitute c = [c]

