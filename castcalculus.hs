data Type = Int
          | Bool
	  | Float
          | FuncT Type Type
          | Dyn
          deriving (Show, Eq)

type Label = String

data Expr = ConstI Int
          | ConstB Bool
	  | ConstF Float
          | VarE String
          | FuncE String Type Expr
          | AppE Expr Expr
          | ExprC Expr Type Type Label
          | Blame Type Label
          | Null
          deriving (Show, Eq)

isGround :: Type -> Bool
isGround Int = True
isGround Bool = True
isGround Float = True
isGround (FuncT Dyn Dyn) = True

isValue :: Expr -> Bool
isValue Null = False
isValue (ConstI x) = True
isValue (ConstB x) = True
isValue (ConstF x) = True
isValue (VarE x) = True
isValue (FuncE x t1 exp) = True
isValue (ExprC v (FuncT t1 t2) (FuncT t3 t4) l) = isValue v
isValue (ExprC v t1 Dyn l)
        | isGround t1 = isValue v
        | otherwise = False

beta :: Expr -> Expr
beta (AppE (FuncE x t f) v)
     | (isValue v) = AppE v f
     | otherwise = Null

idbase :: Expr -> Expr
idbase Null = Null
idbase (ExprC expr t1 t2 l) = if (isValue expr) then expr else Null

idstar :: Expr -> Expr
idstar Null = Null
idstar (ExprC expr Dyn Dyn l) = if (isValue expr) then expr else Null

succeed :: Expr -> Expr -> Expr
succeed Null Null = Null
succeed (ExprC v1 g1 Dyn l1) (ExprC v2 Dyn g2 l2)
        | (isValue v1) && (isGround g1) && (isGround g2) = if g1 == g2 then v1 else (Blame g2 l2)
	| otherwise = Null
	where
	  v1 = v2

appcast :: Expr -> Expr
appcast Null = Null
appcast (AppE (ExprC v1 (FuncT t1 t2) (FuncT t3 t4) l) v2)
        | (isValue v1) && (isValue v2) = AppE v1 (ExprC (ExprC v2 t3 t1 l) t2 t4 l)
	| otherwise = (Blame t4 l)

--ground

--expand

--cong

--blame