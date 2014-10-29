-- Convert GHC's core to our (very similar) IR
--
-- The main changes:
--
-- * Lambda expressions are only allowed at the top level and
--   are distinguished as functions
--
-- * Function application expressions take a function and a list of expressions

module CoreToIR ( coreModuleToIR ) where

import Outputable ( ppr, showSDoc, mkUserStyle, renderWithStyle,
                    neverQualify,
                    Depth(..), QualifyName(..), PprStyle )
  
-- import Unique ( pprUnique )

import qualified IR as RRir
import IRUtils ( combine_namespaces )

import GHC ( CoreModule,  PrintUnqualified, DataCon,  Name, TyCon,
             moduleNameString, nameModule, tyConDataCons, moduleName,
             cm_module, cm_types, cm_binds )
import HscTypes ( typeEnvTyCons )
import TyCon ( isDataTyCon, tyConName )
import DataCon ( dataConName, dataConRepArgTys )
import Var ( varName, varType ) -- varUnique
import Type ( Var, tcView, isPrimitiveType, isFunTy, splitFunTys )
import TypeRep ( Type (..) )
import CoreSyn ( CoreBind, CoreBndr, CoreExpr, Expr(..), AltCon(..),  Bind(..),
                 collectArgs, collectBinders )
import Literal ( Literal(..) )

-- TODO: char literals / char types
import Data.Tuple ( swap )
import Data.List ( partition )
import Data.Maybe ( fromMaybe )

-- Discard rec/nonrec distinctions from bindings
flattenBinding :: CoreBind -> [(Var, CoreExpr)]
flattenBinding (Rec bl) = bl
flattenBinding (NonRec v e) = [(v, e)]

coreModuleToIR :: CoreModule -> (RRir.Module, [(String, RRir.Expr)])
coreModuleToIR cm = (RRir.Module mname ns, varRRExprs)
    where
      mname = moduleNameString $ moduleName $ cm_module cm
      ns = combine_namespaces ghcDefaultNS (funDefs, typeDefs)
      
      -- Convert top-level variable definitions: treat lambda expressions
      -- as functions
      allBindings = concat $ map flattenBinding $ cm_binds cm
      
      -- FIXME: doesn't work with Num a => ...
      -- this should usually be ignored but we should just treat it as an IntType
      (funExprs, varExprs) = partition (isFunTy . varType . fst) allBindings
      funDefs = map toFunDef funExprs
      varRRExprs = map (\(v, e) -> (makeVarId v, makeExpr e)) varExprs 
      

      -- Type translation
      typeDefs :: [(String, RRir.SumType)]
      typeDefs = map makeAlgebraicType $
                 filter isDataTyCon (typeEnvTyCons $ cm_types cm)

      -- Simplify names without our module; qualify those outside
      -- This is the reason everything is defined within coreModuleToIR:
      -- makeId needs to know the name of the current module
      myStyle :: PprStyle
      myStyle = mkUserStyle qualFunctions AllTheWay
        where
          qualFunctions :: PrintUnqualified
          qualFunctions = (qualifyOtherModules, neverPrint)  
          neverPrint _ = False
                                           
          qualifyOtherModules :: Name -> QualifyName
          qualifyOtherModules n | modName == currentModuleName = unqualified
                                | otherwise = qualified
             where
               modName = moduleName $ nameModule n
               currentModuleName = moduleName $ cm_module cm
               unqualified = NameUnqual
               qualified = NameQual $ moduleName $ nameModule n

      -- Make an indentifier from a Name.  All the hard work is above.
      makeId :: Name -> String
      makeId n = renderWithStyle (ppr n) myStyle
      
      makeVarId :: Var -> String
      makeVarId v = makeId $ varName v

      -- Starting from algebraic types only
      makeAlgebraicType :: TyCon -> (String, RRir.SumType)
      makeAlgebraicType tcon = (tname, RRir.SumType dataCons)
      	where
      	  tname = makeId $ tyConName tcon
          dataCons = map makeCdef $ tyConDataCons tcon

      -- Data constructor
      makeCdef :: DataCon -> (String, [RRir.Type])
      makeCdef dc = (dconName, types)
	      where
          dconName = makeId $ dataConName dc
          types = map makeType $ dataConRepArgTys dc
          
      -- Convert a type; obscure ones not supported.  Expand synonyms first
      makeType :: Type -> RRir.Type
      makeType t | Just expanded <- tcView t = makeType expanded
      makeType t | isPrimitiveType t = case shortName of
                    "Int#" -> RRir.IntType
                    "Int" -> RRir.IntType
                    _ -> error $ "unexpected primitive type \"" ++ shortName ++ "\""
                 | otherwise = makeType' t
        where
          shortName = renderWithStyle (ppr t)
                         (mkUserStyle neverQualify AllTheWay)

      makeType' :: Type -> RRir.Type
      makeType' (TyConApp tc []) = 
        if tname `elem` intTys then RRir.IntType else RRir.TypeRef tname
        where
        tname = makeId $ tyConName tc
        intTys = ["GHC.Types.Int"]
      
      makeType' t@(FunTy _ _) =
          error $ "unexpected FunTy type \"" ++ showSDoc (ppr t) ++ "\""
      makeType' t@(TyConApp _ _) = 
          error $ "unexpected TyConApp type with arguments \"" ++
                showSDoc (ppr t) ++ "\""
      makeType' t@(AppTy _ _) =
          error $ "unexpected App type \"" ++ showSDoc (ppr t) ++ "\""
      makeType' t@(TyVarTy _) =
          error $ "unexpected TyVarTy type \"" ++ showSDoc (ppr t) ++ "\""
      makeType' t@(ForAllTy _ _) = 
          error $ "unexpected ForALlTy type \"" ++ showSDoc (ppr t) ++ "\""


      -- Convert to functions: top-level lambdas are expected, but also sometimes
      -- a = (b :: X -> Y).  GHC reduces to this sometimes (examples/AddPrim.hs)
      toFunDef :: (CoreBndr, CoreExpr) -> (String, RRir.Function)
      
      toFunDef (v, e@(Lam _ _)) = (fname, RRir.Function rt args impl)
        where
          fname = makeId $ varName v
          (cArgs, expr) = collectBinders e
          args = map (swap . makeVBind) cArgs
          rt = getReturnType $ varType v
          impl = makeExpr expr
      
      toFunDef (v, Var wrappedFn) = 
      	(newname, RRir.Function rt args (RRir.Call rt wrappedFname vars))
      	where
      	newname = makeVarId v
      	wrappedFname = makeVarId wrappedFn
      	ftype = varType v
      	
      	(argtys', rt') = splitFunTys ftype
      	rt = makeType rt'
      	argtys = map makeType argtys'
      	
      	argnames = map (\i -> "a" ++ show i) ([1..] :: [Integer])
      	args = zip argtys argnames
      	vars = map (uncurry RRir.Var) args

      -- TODO: you may want to handle partial applications similarly to above
      -- TODO: you may want to handle alternate lambda returns (Case) with inlining 
      toFunDef (v, e) = 
        error $ "unknown function definition " ++ 
        showSDoc (ppr v) ++ " = " ++ showSDoc (ppr e)
      	
      -- Get return type of function type; getReturnType (a -> b -> c) = c
      getReturnType :: Type -> RRir.Type
      getReturnType (FunTy _ t) = makeType $ getRT' t
	    where
	      getRT' (FunTy _ rt) = getRT' rt
	      getRT' rt = rt
	  
      getReturnType t =
        error $ "looking for return type of type " ++ showSDoc (ppr t)


      -- Convert an expression.
      --
      -- Nested applications are flattened into applications with
      -- multiple arguments

      -- The ctors to pretend don't exist
      intCtors = ["GHC.Types.I#", "I#"]
      
      makeExpr :: CoreExpr -> RRir.Expr
      makeExpr (Lit l) = RRir.Literal $ makeLit l
      makeExpr (Var v) = RRir.Var (makeType $ varType v) (makeVarId v)
      makeExpr e@(App _ _) = 
          if fname `notElem` noops then call else head args'
          where
            (fun, args) = collectArgs e
            (rt, fname) = case fun of
                     Var v -> (getReturnType $ varType v, makeId $ varName v)
                     _ -> error $ "unrecognized function \"" ++
                          showSDoc (ppr fun) ++ "\""
            args' = map makeExpr args
            fname' = fromMaybe fname $ lookup fname faliases
            call = RRir.Call rt fname' args'

            --TODO: fill these
            faliases = [
                    ("GHC.Base.plusInt", "+"),
                    ("GHC.Base.minusInt", "-"),
                    ("GHC.Base.timesInt", "*"),
                    ("GHC.Prim.+#", "+"),
                    ("GHC.Prim.-#", "-"),
                    ("GHC.Prim.*#", "*"),
                    ("GHC.Prim.>#", ">"),
                    ("GHC.Prim.<#", "<")
                ]
            noops = ["id"] ++ intCtors
            
            
      makeExpr (Let newVars body) = RRir.Let bindings (makeExpr body)
        where
          bindings = map makeBinding flattenedBindings
          makeBinding (b,e) = (fst $ makeVBind b, makeExpr e)
          flattenedBindings = flattenBinding newVars
          
      makeExpr (Case m v _ [(DataAlt dc, [binding], cont)]) 
        | (makeId $ dataConName dc) `elem` intCtors =
        RRir.Let [(vname, makeExpr m)] $
        RRir.Let [(makeVarId binding, RRir.Var RRir.IntType vname)] $
        makeExpr cont
        where vname = makeVarId v
        
        
      makeExpr (Case m v _ alts) = RRir.Case (makeExpr m) var branches
        where
          var = fst (makeVBind v)
          branches = map (\(p, b, e) -> (toPattern p b, makeExpr e)) alts
          toPattern :: AltCon -> [CoreBndr] -> RRir.Pattern
          toPattern DEFAULT _ = RRir.DefaultMatch
          toPattern (LitAlt l) _ = RRir.LiteralMatch (makeLit l)
          toPattern (DataAlt dc) bindings = RRir.LabelMatch label bindings'
              where
                label = makeId $ dataConName dc
                bindings' = map makeVBind bindings
			
      makeExpr e@(Lam _ _) =
          error $ "unallowed Lambda expression \"" ++ showSDoc (ppr e) ++ "\""
      makeExpr e@(Cast _ _) =
          error $ "unallowed Cast expression \"" ++ showSDoc (ppr e) ++ "\""
      makeExpr e@(Tick _ _) =
          error $ "unallowed Tick expression \"" ++ showSDoc (ppr e) ++ "\""
      makeExpr e@(Type _) =
          error $ "unallowed Type expression \"" ++ showSDoc (ppr e) ++ "\""
      makeExpr e@(Coercion _) =
          error $ "unallowed Coercion expression \"" ++ showSDoc (ppr e) ++ "\""


      -- Convert a binder: convert the name and the type
      makeVBind :: CoreBndr -> (String, RRir.Type)
      makeVBind b = (makeVarId b, makeType (varType b))

      -- Convert a literal
      makeLit :: Literal -> RRir.Literal
      makeLit l =
          case l of
            MachInt i -> RRir.IntLiteral i
            MachInt64 i -> RRir.IntLiteral i
            MachWord i -> RRir.IntLiteral i
            MachWord64 i -> RRir.IntLiteral i
            LitInteger i _ -> RRir.IntLiteral i
            --TODO: MatchString s
            _ -> error $ "unsupported literal \"" ++ showSDoc (ppr l) ++ "\""


ghcDefaultNS :: RRir.Namespace
ghcDefaultNS = ([], []) {- (fns, tys)
	where
	fns = [ghc_base_plusInt, ghc_base_timesInt, unlift]
	tys = [ghc_types_int, ghc_types_bool]
	
	ghc_types_int = 
		("GHC.Types.Int", RRir.SumType [
			("GHC.Types.I#", [RRir.IntType])
		])
		
	ghc_types_bool =
		("GHC.Types.Bool", RRir.SumType [
			("GHC.Types.False", []),
			("GHC.Types.True", [])
		])

	itype = RRir.TypeRef "GHC.Types.Int"
	
	ghc_base_intOp op = RRir.Function itype [(itype, "lhs"), (itype, "rhs")] $
		RRir.Case (RRir.Var itype "lhs") "$wild" [
		(RRir.LabelMatch "GHC.Types.I#" [("lhs_", RRir.IntType)],
            RRir.Case (RRir.Var itype "rhs") "$wild1" [
            (RRir.LabelMatch "GHC.Types.I#" [("rhs_", RRir.IntType)],
					let args = [lhs, rhs]
					    lhs = (RRir.Var RRir.IntType "lhs_")
					    rhs = (RRir.Var RRir.IntType "rhs_")
					    primRes = RRir.Call RRir.IntType op args
					    wrappedRes = RRir.Call itype "GHC.Types.I#" [primRes]
					in wrappedRes
            )]
		)]
	
	ghc_base_plusInt = ("GHC.Base.plusInt", ghc_base_intOp "GHC.Prim.+#")
	ghc_base_timesInt = ("GHC.Base.timesInt", ghc_base_intOp "GHC.Prim.*#")
	unlift = ("_unlift", RRir.Function RRir.IntType [(itype, "lifted")] $
	    RRir.Case (RRir.Var itype "lifted") "_" [
	        (RRir.LabelMatch "GHC.Types.I#" [("u", RRir.IntType)], RRir.Var RRir.IntType "u")
	    ])
	-}
