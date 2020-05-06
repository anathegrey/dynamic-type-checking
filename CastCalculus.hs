module CastCalculus where
       import Data

       succeed :: Expr -> Expr
       succeed (ExprC (ExprC v g1 Dyn l1) Dyn g2 l2)
               | (isValue v) && (isGround g1) && (isGround g2) = if g1 == g2 then v else (Blame g2 l2)
               | otherwise = interp (ExprC (ExprC v g1 Dyn l1) Dyn g2 l2)

       appcast :: Expr -> Expr
       appcast (AppE (ExprC v1 (FuncT t1 t2) (FuncT t3 t4) l) v2)
               | (isValue v1) && (isValue v2) = ExprC (AppE v1 (ExprC v2 t3 t1 l)) t2 t4 l
               | otherwise = interp (AppE (ExprC v1 (FuncT t1 t2) (FuncT t3 t4) l) v2)

--incomplete
       buildCompatible :: Type -> Type
       buildCompatible (FuncT t1 t2) = FuncT t1 Dyn

       ground :: Expr -> Expr
       ground (ExprC v t Dyn l)
              | (isValue v) && t /= Dyn && (isGround t) == False = let g = (buildCompatible t) in ExprC (ExprC v t g l) g Dyn l
              | otherwise = if (isValue v) then v else (ExprC v Dyn t l)

       expand :: Expr -> Expr
       expand (ExprC v Dyn t l)
              | (isValue v) && t /= Dyn && (isGround t) == False = let g = (buildCompatible t) in ExprC (ExprC v Dyn g l) g t l
              | otherwise = if (isValue v) then v else (ExprC v Dyn t l)

       subst :: Expr -> String -> Expr -> Expr
       subst (ConstI n Int) x v = (ConstI n Int)
       subst (ConstI n Dyn) x v = (ConstI n Dyn)
       subst (ConstB n Bool) x v = (ConstB n Bool)
       subst (ConstB n Dyn) x v = (ConstB n Dyn)   
       subst (ConstF n Float) x v = (ConstF n Float)
       subst (ConstF n Dyn) x v = (ConstF n Dyn)
       subst (VarE y) x v
             | x == y = v
             | otherwise = (VarE y)
       subst (FuncE y t f) x v
             | x == y = (FuncE y t f)
             | otherwise = (FuncE y t (subst f x v))
       subst (Add e1 e2) x v = Add (subst e1 x v) (subst e2 x v)
       subst (Sub e1 e2) x v = Sub (subst e1 x v) (subst e2 x v)
       subst (Mul e1 e2) x v = Mul (subst e1 x v) (subst e2 x v)
       subst (Div e1 e2) x v = Div (subst e1 x v) (subst e2 x v)
       subst (Less e1 e2) x v = Less (subst e1 x v) (subst e2 x v)
       subst (LessEq e1 e2) x v = LessEq (subst e1 x v) (subst e2 x v)
       subst (Bigger e1 e2) x v = Bigger (subst e1 x v) (subst e2 x v)
       subst (BiggerEq e1 e2) x v = BiggerEq (subst e1 x v) (subst e2 x v)
       subst (If e1 e2 e3) x v = If (subst e1 x v) (subst e2 x v) (subst e3 x v)
       subst (ExprC expr t1 t2 l) x v = (ExprC (subst expr x v) t1 t2 l)
       subst (AppE e1 e2) x v = AppE (subst e1 x v) (subst e2 x v)
       subst (Blame t l) x v = (Blame t l)
       subst None x v = None

--interpretador
       interp :: Expr -> Expr
       interp (AppE (FuncE x t f) v) = if (isValue v) then interp (subst f x v) else interp (AppE (FuncE x t f) (interp v))
       interp (ExprC (ExprC v1 g1 Dyn l1) Dyn g2 l2) = interp (succeed (ExprC (ExprC v1 g1 Dyn l1) Dyn g2 l2))
       interp (ExprC (Blame t l) t1 t2 l1) = if (t == t1) then (Blame t2 l) else interp (ExprC (Blame t l) t1 t2 l1)
       interp (ExprC None t1 t2 l1) = if (t1 == Dyn && t2 /= Dyn) then (Blame t2 l1) else (Blame t1 l1)
       interp (AppE (ExprC v1 (FuncT t1 t2) (FuncT t3 t4) l) v2) = interp (appcast (AppE (ExprC v1 (FuncT t1 t2) (FuncT t3 t4) l) v2))
       interp (AppE expr1 expr2) = (AppE (interp expr1) (interp expr2))
       interp (ExprC expr t1 t2 l)
              | (isValue expr) && t1 == t2 && t1 /= (FuncT Dyn Dyn) && t1 /= Dyn = expr
              | (isValue expr) && t1 == t2 && t1 == Dyn = expr
              | t1 /= Dyn && t2 == Dyn = interp (ground (ExprC (interp expr) t1 Dyn l))
              | t1 == Dyn && t2 /= Dyn = interp (expand (ExprC (interp expr) Dyn t2 l)) 
              | otherwise = interp (ExprC expr t1 t2 l)
       interp (ConstI x Int) = (ConstI x Int)
       interp (ConstI x Dyn) = (ConstI x Dyn)
       interp (ConstB x Bool) = (ConstB x Bool)
       interp (ConstB x Dyn) = (ConstB x Dyn) 
       interp (ConstF x Float) = (ConstF x Float)
       interp (ConstF x Dyn) = (ConstF x Dyn)
       interp (Add e1 e2)
              | isInt expr1 =
                  case () of
                    () | isFloat expr2 -> ConstF (fromInt (takeInt expr1) + takeFloat expr2) Float
                       | isDynFloat expr2 -> ConstF (fromInt (takeInt expr1) + takeFloat expr2) Dyn
                       | isInt expr2 -> ConstI (takeInt expr1 + takeInt expr2) Int
                       | isDynInt expr2 -> ConstI (takeInt expr1 + takeInt expr2) Dyn
              | isFloat expr1 =
                  case () of
                    () | isFloat expr2 -> ConstF (takeFloat expr1 + takeFloat expr2) Float
                       | isDynFloat expr2 -> ConstF (takeFloat expr1 + takeFloat expr2) Dyn
                       | isInt expr2 -> ConstF (takeFloat expr1 + fromInt (takeInt expr2)) Float
                       | isDynInt expr2 -> ConstF (takeFloat expr1 + fromInt (takeInt expr2)) Dyn
              | isDynInt expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstF (fromInt (takeInt expr1) + takeFloat expr2) Dyn
                       | isInt expr2 || isDynInt expr2 -> ConstI (takeInt expr1 + takeInt expr2) Dyn
              | isDynFloat expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstF (takeFloat expr1 + takeFloat expr2) Dyn
                       | isInt expr2 || isDynInt expr2 -> ConstF (takeFloat expr1 + fromInt (takeInt expr2)) Dyn
              where expr1 = interp e1
                    expr2 = interp e2
       interp (Sub e1 e2) 
              | isInt expr1 =
                  case () of
                    () | isFloat expr2 -> ConstF (fromInt (takeInt expr1) - takeFloat expr2) Float
                       | isDynFloat expr2 -> ConstF (fromInt (takeInt expr1) - takeFloat expr2) Dyn
                       | isInt expr2 -> ConstI (takeInt expr1 - takeInt expr2) Int
                       | isDynInt expr2 -> ConstI (takeInt expr1 - takeInt expr2) Dyn
              | isFloat expr1 =
                  case () of
                    () | isFloat expr2 -> ConstF (takeFloat expr1 - takeFloat expr2) Float
                       | isDynFloat expr2 -> ConstF (takeFloat expr1 - takeFloat expr2) Dyn
                       | isInt expr2 -> ConstF (takeFloat expr1 - fromInt (takeInt expr2)) Float
                       | isDynInt expr2 -> ConstF (takeFloat expr1 - fromInt (takeInt expr2)) Dyn
              | isDynInt expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstF (fromInt (takeInt expr1) - takeFloat expr2) Dyn
                       | isInt expr2 || isDynInt expr2 -> ConstI (takeInt expr1 - takeInt expr2) Dyn
              | isDynFloat expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstF (takeFloat expr1 - takeFloat expr2) Dyn
                       | isInt expr2 || isDynInt expr2 -> ConstF (takeFloat expr1 - fromInt (takeInt expr2)) Dyn
              where expr1 = interp e1
                    expr2 = interp e2
       interp (Mul e1 e2) 
              | isInt expr1 =
                  case () of
                    () | isFloat expr2 -> ConstF (fromInt (takeInt expr1) * takeFloat expr2) Float
                       | isDynFloat expr2 -> ConstF (fromInt (takeInt expr1) * takeFloat expr2) Dyn
                       | isInt expr2 -> ConstI (takeInt expr1 * takeInt expr2) Int
                       | isDynInt expr2 -> ConstI (takeInt expr1 * takeInt expr2) Dyn
              | isFloat expr1 =
                  case () of
                    () | isFloat expr2 -> ConstF (takeFloat expr1 * takeFloat expr2) Float
                       | isDynFloat expr2 -> ConstF (takeFloat expr1 * takeFloat expr2) Dyn
                       | isInt expr2 -> ConstF (takeFloat expr1 * fromInt (takeInt expr2)) Float
                       | isDynInt expr2 -> ConstF (takeFloat expr1 * fromInt (takeInt expr2)) Dyn
              | isDynInt expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstF (fromInt (takeInt expr1) * takeFloat expr2) Dyn
                       | isInt expr2 || isDynInt expr2 -> ConstI (takeInt expr1 * takeInt expr2) Dyn
              | isDynFloat expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstF (takeFloat expr1 * takeFloat expr2) Dyn
                       | isInt expr2 || isDynInt expr2 -> ConstF (takeFloat expr1 * fromInt (takeInt expr2)) Dyn
              where expr1 = interp e1
                    expr2 = interp e2
       interp (Div e1 e2)
              | isInt expr1 =
                  case () of
                    () | isFloat expr2 -> ConstF (fromInt (takeInt expr1) / takeFloat expr2) Float
                       | isDynFloat expr2 -> ConstF (fromInt (takeInt expr1) / takeFloat expr2) Dyn
                       | isInt expr2 -> ConstF (fromInt (takeInt expr1) / fromInt (takeInt expr2)) Float
                       | isDynInt expr2 -> ConstF (fromInt (takeInt expr1) / fromInt (takeInt expr2)) Dyn
              | isFloat expr1 =
                  case () of
                    () | isFloat expr2 -> ConstF (takeFloat expr1 / takeFloat expr2) Float
                       | isDynFloat expr2 -> ConstF (takeFloat expr1 / takeFloat expr2) Dyn
                       | isInt expr2 -> ConstF (takeFloat expr1 / fromInt (takeInt expr2)) Float
                       | isDynInt expr2 -> ConstF (takeFloat expr1 / fromInt (takeInt expr2)) Dyn
              | isDynInt expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstF (fromInt (takeInt expr1) / takeFloat expr2) Dyn
                       | isInt expr2 || isDynInt expr2 -> ConstF (fromInt (takeInt expr1) / fromInt (takeInt expr2)) Dyn
              | isDynFloat expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstF (takeFloat expr1 / takeFloat expr2) Dyn
                       | isInt expr2 || isDynInt expr2 -> ConstF (takeFloat expr1 / fromInt (takeInt expr2)) Dyn
              where expr1 = interp e1
                    expr2 = interp e2
       interp (Less e1 e2)
              | isInt expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstB (fromInt (takeInt expr1) < takeFloat expr2) Bool
                       | isInt expr2 || isDynInt expr2 -> ConstB (takeInt expr1 < takeInt expr2) Bool
              | isFloat expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstB (takeFloat expr1 < takeFloat expr2) Bool
                       | isInt expr2 || isDynInt expr2 -> ConstB (takeFloat expr1 < fromInt (takeInt expr2)) Bool
              | isDynInt expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstB (fromInt (takeInt expr1) < takeFloat expr2) Bool
                       | isInt expr2 || isDynInt expr2 -> ConstB (takeInt expr1 < takeInt expr2) Bool
              | isDynFloat expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstB (takeFloat expr1 < takeFloat expr2) Bool
                       | isInt expr2 || isDynInt expr2 -> ConstB (takeFloat expr1 < fromInt (takeInt expr2)) Bool
              where expr1 = interp e1
                    expr2 = interp e2
       interp (Bigger e1 e2)
              | isInt expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstB (fromInt (takeInt expr1) > takeFloat expr2) Bool
                       | isInt expr2 || isDynInt expr2 -> ConstB (takeInt expr1 > takeInt expr2) Bool
              | isFloat expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstB (takeFloat expr1 > takeFloat expr2) Bool
                       | isInt expr2 || isDynInt expr2 -> ConstB (takeFloat expr1 > fromInt (takeInt expr2)) Bool
              | isDynInt expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstB (fromInt (takeInt expr1) > takeFloat expr2) Bool
                       | isInt expr2 || isDynInt expr2 -> ConstB (takeInt expr1 > takeInt expr2) Bool
              | isDynFloat expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstB (takeFloat expr1 > takeFloat expr2) Bool
                       | isInt expr2 || isDynInt expr2 -> ConstB (takeFloat expr1 > fromInt (takeInt expr2)) Bool
              where expr1 = interp e1
                    expr2 = interp e2
       interp (LessEq e1 e2)
              | isInt expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstB (fromInt (takeInt expr1) <= takeFloat expr2) Bool
                       | isInt expr2 || isDynInt expr2 -> ConstB (takeInt expr1 <= takeInt expr2) Bool
              | isFloat expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstB (takeFloat expr1 <= takeFloat expr2) Bool
                       | isInt expr2 || isDynInt expr2 -> ConstB (takeFloat expr1 <= fromInt (takeInt expr2)) Bool
              | isDynInt expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstB (fromInt (takeInt expr1) <= takeFloat expr2) Bool
                       | isInt expr2 || isDynInt expr2 -> ConstB (takeInt expr1 <= takeInt expr2) Bool
              | isDynFloat expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstB (takeFloat expr1 <= takeFloat expr2) Bool
                       | isInt expr2 || isDynInt expr2 -> ConstB (takeFloat expr1 <= fromInt (takeInt expr2)) Bool
              where expr1 = interp e1
                    expr2 = interp e2
       interp (Eq e1 e2)
              | isInt expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstB (fromInt (takeInt expr1) == takeFloat expr2) Bool
                       | isInt expr2 || isDynInt expr2 -> ConstB (takeInt expr1 == takeInt expr2) Bool
              | isFloat expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstB (takeFloat expr1 == takeFloat expr2) Bool
                       | isInt expr2 || isDynInt expr2 -> ConstB (takeFloat expr1 == fromInt (takeInt expr2)) Bool
              | isDynInt expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstB (fromInt (takeInt expr1) == takeFloat expr2) Bool
                       | isInt expr2 || isDynInt expr2 -> ConstB (takeInt expr1 == takeInt expr2) Bool
              | isDynFloat expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstB (takeFloat expr1 == takeFloat expr2) Bool 
                       | isInt expr2 || isDynInt expr2 -> ConstB (takeFloat expr1 == fromInt (takeInt expr2)) Bool
              where expr1 = interp e1
                    expr2 = interp e2
       interp (BiggerEq e1 e2)
              | isInt expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstB (fromInt (takeInt expr1) >= takeFloat expr2) Bool
                       | isInt expr2 || isDynInt expr2 -> ConstB (takeInt expr1 >= takeInt expr2) Bool
              | isFloat expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstB (takeFloat expr1 >= takeFloat expr2) Bool
                       | isInt expr2 || isDynInt expr2 -> ConstB (takeFloat expr1 >= fromInt (takeInt expr2)) Bool
              | isDynInt expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstB (fromInt (takeInt expr1) >= takeFloat expr2) Bool
                       | isInt expr2 || isDynInt expr2 -> ConstB (takeInt expr1 >= takeInt expr2) Bool
              | isDynFloat expr1 =
                  case () of
                    () | isFloat expr2 || isDynFloat expr2 -> ConstB (takeFloat expr1 >= takeFloat expr2) Bool
                       | isInt expr2 || isDynInt expr2 -> ConstB (takeFloat expr1 >= fromInt (takeInt expr2)) Bool
              where expr1 = interp e1
                    expr2 = interp e2
       interp (If e1 e2 e3) = if (takeBool (interp e1)) then (interp e2) else (interp e3)
       interp None = None


--Examples

       ex1 :: Expr
       ex1 = ExprC (AppE (FuncE "y" Float (If (Bigger (VarE "y") (ConstI 0 Int)) (Sub (Sub (Mul (VarE "y") (ConstI 3 Int)) (VarE "y")) (ConstI 1 Int)) None)) (ExprC (ConstF 3.01 Float) Float Dyn "l")) Dyn Float "l"

       ex2 :: Expr
       ex2 = ExprC (AppE (FuncE "y" Float (If (Bigger (VarE "y") (ConstI 0 Int)) (Sub (Sub (Mul (VarE "y") (ConstI 3 Int)) (VarE "y")) (ConstI 1 Int)) None)) (ExprC (ConstF (-2.99) Float) Float Dyn "l")) Dyn Float "l"

       ex3 :: Expr
       ex3 = AppE (ExprC (FuncE "y" Int (Add (VarE "y") (ConstI 1 Int))) (FuncT Int Int) (FuncT Dyn Dyn) "l") (ConstI 2 Dyn) 