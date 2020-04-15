data Type = Int
          | Bool
          | Float
          | FuncT Type Type
          | Dyn
          | NoneType
          deriving (Show, Eq)

type Label = String

data Expr = ConstI Int
          | ConstB Bool
          | ConstF Float
          | VarE String
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Less Expr Expr
          | Bigger Expr Expr
          | LessEq Expr Expr
          | BiggerEq Expr Expr
          | If Expr Expr Expr
          | FuncE String Type Expr
          | AppE Expr Expr
          | ExprC Expr Type Type Label
          | Blame Type Label
          | None
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
isValue None = True
isValue (ExprC v (FuncT t1 t2) (FuncT t3 t4) l) = isValue v
isValue (ExprC v t1 Dyn l)
        | isGround t1 = isValue v
        | otherwise = False

subs :: Expr -> String -> Expr -> Expr

beta :: Expr -> Expr
beta (AppE (FuncE x t f) v) = if (isValue v) then (FuncE x t (subs f x v)) else (AppE (FuncE x t f) v)

succeed :: Expr -> Expr
succeed (ExprC (ExprC v g1 Dyn l1) Dyn g2 l2)
        | (isValue v) && (isGround g1) && (isGround g2) = if g1 == g2 then v else (Blame g2 l2)
        | otherwise = (ExprC (ExprC v g1 Dyn l1) Dyn g2 l2)

appcast :: Expr -> Expr
appcast (AppE (ExprC v1 (FuncT t1 t2) (FuncT t3 t4) l) v2)
        | (isValue v1) && (isValue v2) = (ExprC (AppE v1 (ExprC v2 t3 t1 l)) t2 t4 l)
        | otherwise = (AppE (ExprC v1 (FuncT t1 t2) (FuncT t3 t4) l) v2)

ground :: Expr -> Expr
ground (ExprC v t Dyn l)
       | (isValue v) && t /= Dyn && t /= Int = ExprC (ExprC v t Int l) Int Dyn l
       | (isValue v) && t /= Dyn && t /= Bool = ExprC (ExprC v t Bool l) Bool Dyn l
       | (isValue v) && t /= Dyn && t /= Float = ExprC (ExprC v t Float l) Float Dyn l
       | (isValue v) && t /= Dyn && t /= (FuncT Dyn Dyn) = ExprC (ExprC v t (FuncT Dyn Dyn) l) (FuncT Dyn Dyn) Dyn l
       | otherwise = ExprC v t Dyn l

expand :: Expr -> Expr
expand (ExprC v Dyn t l) 
       | (isValue v) && t /= Dyn && t /= Int = ExprC (ExprC v t Int l) Int Dyn l
       | (isValue v) && t /= Dyn && t /= Bool = ExprC (ExprC v t Bool l) Bool Dyn l
       | (isValue v) && t /= Dyn && t /= Float = ExprC (ExprC v t Float l) Float Dyn l
       | (isValue v) && t /= Dyn && t /= (FuncT Dyn Dyn) = ExprC (ExprC v t (FuncT Dyn Dyn) l) (FuncT Dyn Dyn) Dyn l
       | otherwise = ExprC v Dyn t l

takeInt :: Expr -> Int
takeInt (ConstI n) = n

takeFloat :: Expr -> Float
takeFloat (ConstF n) = n

takeBool :: Expr -> Bool
takeBool (ConstB n) = n

interp :: Expr -> Expr
interp (AppE (FuncE x t f) v) = beta (AppE (FuncE x t f) v)
interp (ExprC (ExprC v1 g1 Dyn l1) Dyn g2 l2) = succeed (ExprC (ExprC v1 g1 Dyn l1) Dyn g2 l2)
interp (ExprC (Blame t l) t1 t2 l1) = if (t == t1) then (Blame t2 l) else (ExprC (Blame t l) t1 t2 l1)
interp (AppE (ExprC v1 (FuncT t1 t2) (FuncT t3 t4) l) v2) = appcast (AppE (ExprC v1 (FuncT t1 t2) (FuncT t3 t4) l) v2)
interp (AppE expr1 expr2)
       | (isValue expr1) && (isValue expr2) == False = (AppE expr1 (interp expr2))
       | (isValue expr1) == False && (isValue expr2) = (AppE (interp expr1) expr2)
       | (isValue expr1) == False && (isValue expr2) == False = (AppE (interp expr1) (interp expr2))
interp (ExprC expr t1 t2 l)
       | (isValue expr) && t1 == t2 && t1 /= (FuncT Dyn Dyn) && t1 /= Dyn = expr
       | (isValue expr) && t1 == t2 && t1 == Dyn = expr
       | t1 /= Dyn && t2 == Dyn = ground (ExprC (interp expr) t1 Dyn l)
       | t1 == Dyn && t2 /= Dyn = expand (ExprC (interp expr) Dyn t2 l)
       | otherwise = (ExprC expr t1 t2 l)
interp (ConstI x) = (ConstI x)
interp (ConstB x) = (ConstB x)
interp (ConstF x) = (ConstF x)
interp (Add e1 e2) = ConstF (takeFloat (interp e1) + takeFloat (interp e2))
interp (Sub e1 e2) = ConstF (takeFloat (interp e1) - takeFloat (interp e2))
interp (Mul e1 e2) = ConstF (takeFloat (interp e1) * takeFloat (interp e2))
interp _ = None


--Examples

ex1 :: Expr
ex1 = AppE (ExprC (FuncE "y" Float (Sub (VarE "y") (ConstF 1.0))) (FuncT Dyn Dyn) (FuncT Float Float) "l") (ConstF 3.01)

ex2 :: Expr
ex2 = ExprC (AppE (FuncE "y" Float (Sub (VarE "y") (ConstF 1.0))) (ExprC (ConstF 3.01) Float Dyn "l")) Dyn Float "l"