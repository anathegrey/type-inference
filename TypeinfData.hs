module TypeinfData where
       data Expr = VarE String
                 | Lambda String Expr Type
                 | AppE Expr Expr
                 | TypeE Expr Type
                 deriving (Show, Eq)

       data Type = VarT String
                 | AppT Type Type
                 deriving (Show, Eq)

       data Basis = Gama String Type
                  deriving (Show, Eq)

       boundVar :: Expr -> [String]
       boundVar (VarE x) = []
       boundVar (Lambda x expr t) = [x] ++ (boundVar expr)
       boundVar (AppE e1 e2) = (boundVar e1) ++ (boundVar e2)
       boundVar (TypeE expr t) = (boundVar expr)
       
       freeVar :: Expr -> [String]
       freeVar (VarE x) = [x]
       freeVar (Lambda x expr t) = except x (freeVar expr)
       freeVar (AppE e1 e2) = (freeVar e1) ++ (freeVar e2)
       freeVar (TypeE expr t) = (freeVar expr)

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
       
       remove :: [Basis] -> String -> Type -> [Basis]
       remove [] _ _ = []
       remove ((Gama y t1) : bs) x t2 = if y == x && t1 == t2 then bs else [Gama y t1] ++ (remove bs x t2)
       