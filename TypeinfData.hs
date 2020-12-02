module TypeinfData where
       data Expr = VarE String
                 | Lambda String Expr
                 | AppE Expr Expr
                 deriving (Show, Eq)

       data Type = VarT String
                 | AppT Type Type
                 | NoType
                 deriving (Show, Eq)

       data Basis = Gama String Type
                  deriving (Show, Eq)

       boundVar :: Expr -> [String]
       boundVar (VarE x) = []
       boundVar (Lambda x expr) = [x] ++ (boundVar expr)
       boundVar (AppE e1 e2) = (boundVar e1) ++ (boundVar e2)
       
       freeVar :: Expr -> [String]
       freeVar (VarE x) = [x]
       freeVar (Lambda x expr) = except x (freeVar expr)
       freeVar (AppE e1 e2) = (freeVar e1) ++ (freeVar e2)

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

       searchType :: Type -> [(Type, Type)] -> Bool
       searchType _ [] = False
       searchType t ((t1, t2) : ts) = if t == t1 || t == t2 then True else (searchType t ts)
       
       remove :: [Basis] -> String -> [Basis]
       remove [] _ = []
       remove ((Gama y t) : bs) x = if y == x then bs else [Gama y t] ++ (remove bs x)

       count :: Int -> Int
       count c = c + 1

       getType :: String -> [Basis] -> Type
       getType _  [] = NoType
       getType x ((Gama y t) : g) = if x == y then t else (getType x g)