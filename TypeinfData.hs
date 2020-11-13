module TypeinfData where
       data Expr = VarE String
                 | Lambda String Expr
                 | AppE Expr Expr
                 deriving (Show, Eq)

       data Type = VarT String
                 | AppT Type Type
		 | ErrorType
                 deriving (Show, Eq)

       data Basis = Gama String Type
                  deriving (Show, Eq)

       data Set = Equal Type Type
                | Fail
                deriving (Show, Eq)

       boundVar :: Expr -> [String]
       boundVar (VarE x) = []
       boundVar (Lambda x expr) = [x] ++ (boundVar expr)
       boundVar (AppE e1 e2) = (boundVar e1) ++ (boundVar e2)
       
       freeVar :: Expr -> [String]
       freeVar (VarE x) = [x]
       freeVar (Lambda x expr) = except x (freeVar expr)
       freeVar (AppE e1 e2) = (freeVar e1) ++ (freeVar e2)

       --freeVarSet :: [Set] -> [String]
       --freeVarSet :: 

       except :: String -> [String] -> [String]
       except _ [] = []
       except x (y : ys) = if x == y then (except x ys) else y : (except x ys)

       intersect :: [String] -> [String] -> [String]
       intersect l [] = []
       intersect [] l = []
       intersect (x : xs) ys = if elem x ys == False then x : (intersect xs ys) else (intersect xs ys)

       search :: [Basis] -> String -> Bool
       search [] _ = False
       search ((Gama y t) : bs) x = if y == x then True else (search bs x)
       
       remove :: [Basis] -> String -> [Basis]
       remove [] _ = []
       remove ((Gama y t) : bs) x = if y == x then bs else [Gama y t] ++ (remove bs x)

       count :: Int -> Int
       count c = c + 1

       getType :: [Basis] -> [String] -> [Type]
       getType [] _ = []
       getType _ [] = []
       getType ((Gama y t) : bs) (x : xs) = if y == x then [t] ++ (getType bs xs) else (getType bs (x : xs))

       turnSet :: [Type] -> [Type] -> [Set]
       turnSet [] _ = []
       turnSet _ [] = []
       turnSet (t1 : ts) (t2 : tss) = (Equal t1 t2) : (turnSet ts tss)