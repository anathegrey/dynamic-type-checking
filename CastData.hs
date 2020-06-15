module CastData where

       data Type = TInt
                 | TBool
                 | TFloat
                 | FuncT Type Type
                 | Dyn
                 | NoneType
                 deriving (Show, Eq)

       type Label = String

       data Expr = ConstI Int Type
                 | ConstB Bool Type
                 | ConstF Float Type
                 | Minus Expr
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
       isGround TInt = True
       isGround TBool = True
       isGround TFloat = True
       isGround (FuncT Dyn Dyn) = True
       isGround _ = False

       isValue :: Expr -> Bool
       isValue (ConstI x TInt) = True
       isValue (Minus (ConstI x TInt)) = True
       isValue (ConstI x Dyn) = True
       isValue (Minus (ConstI x Dyn)) = True
       isValue (ConstB x TBool) = True
       isValue (ConstB x Dyn) = True
       isValue (ConstF x TFloat) = True
       isValue (Minus (ConstF x TFloat)) = True
       isValue (ConstF x Dyn) = True
       isValue (Minus (ConstF x Dyn)) = True
       isValue (VarE x) = True
       isValue (FuncE x t1 exp) = True
       isValue (ExprC v (FuncT t1 t2) (FuncT t3 t4) l) = True
       isValue (ExprC v t1 Dyn l) = if (isGround t1) then True else False
       isValue _ = False

       takeInt :: Expr -> Int
       takeInt (ConstI n TInt) = n
       takeInt (Minus (ConstI n TInt)) = (-n)
       takeInt (ConstI n Dyn) = n
       takeInt (Minus (ConstI n Dyn)) = (-n)

       isInt :: Expr -> Bool
       isInt (ConstI n TInt) = True
       isInt (Minus (ConstI n TInt)) = True
       isInt _ = False

       fromInt :: Int -> Float
       fromInt n = fromInteger (toInteger n)

       takeFloat :: Expr -> Float
       takeFloat (ConstF n TFloat) = n
       takeFloat (Minus (ConstF n TFloat)) = (-n)
       takeFloat (ConstF n Dyn) = n
       takeFloat (Minus (ConstF n Dyn)) = (-n)

       isFloat :: Expr -> Bool
       isFloat (ConstF n TFloat) = True
       isFloat (Minus (ConstF n TFloat)) = True
       isFloat _ = False
   
       takeBool :: Expr -> Bool
       takeBool (ConstB n TBool) = n
       takeBool (ConstB n Dyn) = n

       isBool :: Expr -> Bool
       isBool (ConstB n TBool) = True
       isBool _ = False

       isDynInt :: Expr -> Bool
       isDynInt (ConstI n Dyn) = True
       isDynInt (Minus (ConstI n Dyn)) = True
       isDynInt _ = False

       isDynFloat :: Expr -> Bool
       isDynFloat (ConstF n Dyn) = True
       isDynFloat (Minus (ConstF n Dyn)) = True
       isDynFloat _ = False

       isDynBool :: Expr -> Bool
       isDynBool (ConstB n Dyn) = True
       isDynBool _ = False
