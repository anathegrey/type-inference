module TypeinfData where
       data Expr = VarE String
                 | Lambda String Expr
                 | AppE Expr Expr
                 deriving (Show, Eq)

       data Type = VarT String
                 | AppT Type Type
                 deriving (Show, Eq)

       data Basis = Gama String Type
                  deriving (Show, Eq)

       data Result = Substitution [(Type, Type)]
                   | FAIL
                   deriving (Show, Eq)

       boundVar :: Expr -> [String]
       boundVar (VarE x) = []
       boundVar (Lambda x expr) = [x] ++ (boundVar expr)
       boundVar (AppE e1 e2) = (boundVar e1) ++ (boundVar e2)
       
       freeVar :: Expr -> [String]
       freeVar (VarE x) = [x]
       freeVar (Lambda x expr) = except x (freeVar expr)
       freeVar (AppE e1 e2) = (freeVar e1) ++ (freeVar e2)

       freeVarType :: Type -> [String]
       freeVarType (VarT t) = [t]
       freeVarType (AppT t1 t2) = (freeVarType t1) ++ (freeVarType t2)

       notMember :: Type -> [String] -> Bool
       notMember _ [] = True
       notMember (VarT t) s = if t == (head s) then False else (notMember (VarT t) (tail s))

       concatResult :: (Type, Type) -> Result -> Result
       concatResult (t1, t2) (Substitution ts) = Substitution ((t1, t2) : ts) 

       except :: String -> [String] -> [String]
       except _ [] = []
       except x (y : ys) = if x == y then (except x ys) else y : (except x ys)

       intersect :: [String] -> [String] -> [String]
       intersect l [] = []
       intersect [] l = []
       intersect (x : xs) ys = if elem x ys == True then x : (intersect xs ys) else (intersect xs ys)

       searchBasis :: [Basis] -> String -> Bool
       searchBasis [] _ = False
       searchBasis ((Gama y t) : bs) x = if y == x then True else (searchBasis bs x)
       
       remove :: [Basis] -> String -> [Basis]
       remove [] _ = []
       remove ((Gama y t) : bs) x = if y == x then (remove bs x) else [Gama y t] ++ (remove bs x)

       count :: Int -> Int
       count c = c + 1

       getType :: String -> [Basis] -> Type
       getType x ((Gama y t) : g) = if x == y then t else (getType x g)