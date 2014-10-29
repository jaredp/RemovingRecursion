
module IR
where

data Module = Module { mname :: String, mnamespace :: Namespace }
			deriving (Show)
			
data SumType = SumType [(Label, [Type])] deriving (Show, Eq, Ord)
type Namespace = ([(String, Function)], [(String, SumType)])

type Label = String

data Type = IntType
		  | TypeRef String
		  deriving (Show, Eq, Ord)

data Expr = Call Type String [Expr]
		  | Case Expr String [(Pattern, Expr)]
		  | Let [(String, Expr)] Expr
		  | Var Type String
		  | Literal Literal
		  deriving (Show, Eq, Ord)
	
data Literal = IntLiteral Integer
			 | SumLiteral String Label [Literal]
			 deriving (Show, Eq, Ord)
			 
data Pattern = LiteralMatch Literal
			 | LabelMatch Label [(String, Type)]	-- bindings
			 | DefaultMatch
			 deriving (Show, Eq, Ord)

data Function = Function {
					funcReturnType :: Type,
					funcArgs :: [(Type, String)],
					funcImpl :: Expr
				} deriving (Show, Eq, Ord)

