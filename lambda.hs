data Term = Var String
     	  | Func String Term
	  | App Term Term
	  deriving (Show, Eq)

--bound variables
bound :: Term -> [String]
bound (Var x) = []
bound (Func x m) = bound m ++ [x]
bound (App m n) = bound m ++ bound n

--free variables
remove :: String -> [String] -> [String]
remove _ [] = []
remove y (x : xs) = if y == x then remove y xs else [x] ++ remove y xs
       
free :: Term -> [String]
free (Var x) = [x] --corrigir
free (Func x m) = remove x (free m)
free (App m n) = free m ++ free n

--substitution 
subs :: Term -> Term -> String -> Term
subs (Var x) l y = if x == y then l else (Var x)
subs (Func x m) l y = if x == y then (Func x m) else (Func x (subs m l y))
subs (App m n) l y = App (subs m l y) (subs n l y)

--beta-conversion (rever)
intersect :: [String] -> [String] -> [String]
intersect [] _ = []
intersect (x : xs) y
	  | elem x y = [x] ++ intersect xs y
	  | otherwise = intersect xs y

betaConv :: Term -> Term
betaConv (Var x) = Var x
betaConv (Func x m) = Func x m
betaConv (App (Func x m) n) = if (intersect (bound (Func x m)) (free n)) == [] then subs m n x else (App (Func x m) n)

--reductions 
reduction :: Term -> Term
reduction (Var x) = Var x
reduction (Func x m) = Func x (reduction m)
reduction (App m n) = betaConv (App (reduction m) (reduction n))
