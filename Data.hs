module Data where

       data Type = Int
                 | Bool
                 | Float
                 | FuncT Type Type
                 | Dyn
                 | NoneType
                 deriving (Show, Eq)

       type Label = String

       data Expr = ConstI Int Type
                 | ConstB Bool Type
                 | ConstF Float Type
                 | VarE String
                 | Add Expr Expr
                 | Mul Expr Expr
                 | Sub Expr Expr
                 | Div Expr Expr
                 | Less Expr Expr
                 | Bigger Expr Expr
                 | LessEq Expr Expr
                 | BiggerEq Expr Expr
                 | Eq Expr Expr
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
       isGround _ = False

       isValue :: Expr -> Bool
       isValue (ConstI x Int) = True
       isValue (ConstI x Dyn) = True
       isValue (ConstB x Bool) = True
       isValue (ConstB x Dyn) = True
       isValue (ConstF x Float) = True
       isValue (ConstF x Dyn) = True
       isValue (VarE x) = True
       isValue (FuncE x t1 exp) = True
       isValue (ExprC v (FuncT t1 t2) (FuncT t3 t4) l) = True
       isValue (ExprC v t1 Dyn l) = if (isGround t1) then True else False
       isValue _ = False

       takeInt :: Expr -> Int
       takeInt (ConstI n Int) = n
       takeInt (ConstI n Dyn) = n

       isInt :: Expr -> Bool
       isInt (ConstI n Int) = True
       isInt (ConstI n _) = False
       isInt (ConstF n _) = False
       isInt (ConstB n _) = False

       fromInt :: Int -> Float
       fromInt n = fromInteger (toInteger n)

       takeFloat :: Expr -> Float
       takeFloat (ConstF n Float) = n
       takeFloat (ConstF n Dyn) = n

       isFloat :: Expr -> Bool
       isFloat (ConstF n Float) = True
       isFloat (ConstF n _) = False
       isFloat (ConstI n _) = False
       isFloat (ConstB n _) = False

       takeBool :: Expr -> Bool
       takeBool (ConstB n Bool) = n
       takeBool (ConstB n Dyn) = n

       isBool :: Expr -> Bool
       isBool (ConstB n Bool) = True
       isBool (ConstI n _) = False
       isBool (ConstF n _) = False

       isDynInt :: Expr -> Bool
       isDynInt (ConstI n Dyn) = True
       isDynInt _ = False

       isDynFloat :: Expr -> Bool
       isDynFloat (ConstF n Dyn) = True
       isDynFloat _ = False

       isDynBool :: Expr -> Bool
       isDynBool (ConstB n Dyn) = True
       isDynBool _ = False