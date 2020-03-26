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
          deriving (Show, Eq)

isGround :: Type -> Bool
isGround Int = True
isGround Bool = True
isGround Float = True
isGround (FuncT Dyn Dyn) = True

isValue :: Expr -> Bool
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
     | otherwise = (AppE (FuncE x t f) v)

succeed :: Expr -> Expr
succeed (ExprC (ExprC v g1 Dyn l1) Dyn g2 l2)
        | (isValue v) && (isGround g1) && (isGround g2) = if g1 == g2 then v else (Blame g2 l2)
	| otherwise = (ExprC (ExprC v g1 Dyn l1) Dyn g2 l2)

appcast :: Expr -> Expr
appcast (AppE (ExprC v1 (FuncT t1 t2) (FuncT t3 t4) l) v2)
        | (isValue v1) && (isValue v2) = AppE v1 (ExprC (ExprC v2 t3 t1 l) t2 t4 l)
	| otherwise = (AppE (ExprC v1 (FuncT t1 t2) (FuncT t3 t4) l) v2)

ground :: Expr -> Expr
ground (ExprC v t Dyn l)
       | (isValue v) && t /= Dyn && t /= Int = ExprC (ExprC v t Int l) Int Dyn l
       | (isValue v) && t /= Dyn && t /= Bool = ExprC (ExprC v t Int l) Bool Dyn l
       | (isValue v) && t /= Dyn && t /= Float = ExprC (ExprC v t Int l) Float Dyn l
       | (isValue v) && t /= Dyn && t /= (FuncT Dyn Dyn) = ExprC (ExprC v t Int l) (FuncT Dyn Dyn) Dyn l
       | otherwise = ExprC v t Dyn l

expand :: Expr -> Expr
expand (ExprC v Dyn t l) 
       | (isValue v) && t /= Dyn && t /= Int = ExprC (ExprC v t Int l) Int Dyn l
       | (isValue v) && t /= Dyn && t /= Bool = ExprC (ExprC v t Int l) Bool Dyn l
       | (isValue v) && t /= Dyn && t /= Float = ExprC (ExprC v t Int l) Float Dyn l
       | (isValue v) && t /= Dyn && t /= (FuncT Dyn Dyn) = ExprC (ExprC v t Int l) (FuncT Dyn Dyn) Dyn l
       | otherwise = ExprC v Dyn t l

interp :: Expr -> Expr
interp (AppE (FuncE x t f) v) = beta (AppE (FuncE x t f) v)
interp (ExprC expr t1 t2 l) = if ((isValue expr) && t1 /= Dyn && t2 /= Dyn) then expr else (ExprC expr t1 t2 l)
interp (ExprC expr Dyn Dyn l) = if (isValue expr) then expr else (ExprC expr Dyn Dyn l)
interp (ExprC (ExprC v1 g1 Dyn l1) Dyn g2 l2) = succeed (ExprC (ExprC v1 g1 Dyn l1) Dyn g2 l2)
interp (AppE (ExprC v1 (FuncT t1 t2) (FuncT t3 t4) l) v2) = appcast (AppE (ExprC v1 (FuncT t1 t2) (FuncT t3 t4) l) v2)
interp (ExprC v t Dyn l) = ground (ExprC v t Dyn l)
interp (ExprC v Dyn t l) = expand (ExprC v Dyn t l)
interp (AppE expr1 expr2) = if (isValue expr1) then (AppE expr1 (interp expr2)) else (AppE (interp expr1) expr2)
interp (ExprC expr t1 t2 l) = ExprC (interp expr) t1 t2 l
interp (ExprC (Blame t l) t1 t2 l1) = if (t == t1) then Blame t2 l else (ExprC (Blame t l) t1 t2 l1)