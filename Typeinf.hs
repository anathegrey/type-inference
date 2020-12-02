module Typeinf where
       import TypeinfData

       subst :: (Type, Type) -> [(Type, Type)] -> [(Type, Type)]
       subst _ [] = []
       subst (alpha, t) ((t1, t2) : ts)
             | (searchType alpha ts) == True = [(NoType, NoType)]
             | otherwise =
                  case () of
                    () | t1 == t   -> (alpha, t2) : (subst (alpha, t) ts)
                       | t2 == t   -> (t1, alpha) : (subst (alpha, t) ts)
                       | otherwise -> (t1, t2) : (subst (alpha, t) ts)

       unify :: [(Type, Type)] -> (Char, Int) -> [(Type, Type)] 
       unify [] _ = []
       unify (((AppT e1 e2), (AppT e3 e4)) : ts) (s, oc) = (unify ((e1, e3) : (e2, e4) : ts) (s, oc))
       unify ((t1, t2) : ts) (s, oc)
             | t1 == t2 = (unify ts (s, oc))
             | t2 == (VarT (s : show oc)) = (unify ((t2, t1) : ts) (s, oc)) 
             | t1 == (VarT (s : show oc)) = (t1, t2) : (unify (subst (t1, t2) ts) (s, oc))
             | otherwise = ((t1, t2) : ts)

       substBasis :: [(Type, Type)] -> [Basis] -> [Basis]
       substBasis [] _ = []
       substBasis _ [] = []
       substBasis ((t1, t2) : ts) ((Gama x tx) : gs)
                  | tx == t2 = (Gama x t1) : (substBasis ((t1, t2) : ts) gs)
                  | otherwise = substBasis ts ((Gama x tx) : gs)


       {-substType :: [(Type, Type)] -> Type -> Type
       substType ((t1, t2) : ts) alpha = if t1 == alpha then (t1, t2) : (substType ts alpha) else (t1, t2) : (substType ts alpha)
       -}

       vFind :: [String] -> [Basis] -> [Basis] -> [(Type, Type)]
       vFind [] _ _ = []
       vFind (x : xs) ((Gama y1 t1) : g1) ((Gama y2 t2) : g2) = if (search ((Gama y1 t1) : g1) x) == True && (search ((Gama y2 t2) : g2) x) == True then (getType x ((Gama y1 t1) : g1),  getType x ((Gama y2 t2) : g2)) : (vFind xs ((Gama y1 t1) : g1) ((Gama y2 t2) : g2)) else (vFind xs ((Gama y1 t1) : g1) ((Gama y2 t2) : g2)) 

       milner :: Expr -> Int -> Char -> ([Basis], Type, Int, Char)
       milner (VarE x) oc s = ([Gama x (VarT (s : show oc))], (VarT (s : show oc)), oc, s)
       milner (Lambda x expr) oc s = if (search g x) == False then (g, (AppT (VarT (s : show oc)) t), oc, s) else ((remove g x), AppT (VarT "t") (VarT (s : show oc)), oc, s)
                                     where (g, t, c, s1) = (milner expr (count oc) s)
       milner (AppE m1 m2) oc s =
                               let v = intersect (freeVar m1) (freeVar m2)
                                   ss = unify ((vFind v g1 g2) ++ [(t1, AppT t2 (VarT (s : (show oc))))]) (s, oc)
                                   (g1, t1, c1, a1) = (milner m1 (count oc) s)
                                   (g2, t2, c2, a2) = (milner m2 (count oc) 'a')
                               in ((substBasis ss g1 ++ g2), (VarT (s : (show oc))), oc, s)

--Examples

       ex1 :: Expr
       ex1 = Lambda "x" (Lambda "y" (Lambda "z" (AppE (VarE "x") (AppE (VarE "y") (VarE "z"))))) 





