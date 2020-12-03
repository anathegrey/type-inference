module Typeinf where
       import TypeinfData

       subst :: (Type, Type) -> [(Type, Type)] -> [(Type, Type)]
       subst _ [] = []
       subst (alpha, t) ((t1, t2) : ts)
             | (searchType alpha ts) == True = [(NoType, NoType)]
             | otherwise =
                  case () of
                    () | t1 == t   -> (t, t2) : (subst (alpha, t) ts)
                       | t2 == t   -> (t1, t) : (subst (alpha, t) ts)
                       | otherwise -> (t1, t2) : (subst (alpha, t) ts)

       unify :: [(Type, Type)] -> (Char, Int) -> [(Type, Type)] 
       unify [] _ = []
       unify (((AppT e1 e2), (AppT e3 e4)) : ts) (s, oc) = (unify ((e1, e3) : (e2, e4) : ts) (s, oc))
       unify ((t1, t2) : ts) (s, oc)
             | t1 == t2 = (unify ts (s, oc))
             | t2 == (VarT (s : show oc)) = (unify ((t2, t1) : ts) (s, oc)) 
             | t1 == (VarT (s : show oc)) = (t1, t2) : (unify (subst (t1, t2) ts) (s, oc))
             | otherwise = (t1, t2) : unify ts (s, oc)

       substBasis :: [(Type, Type)] -> [Basis] -> [Basis]
       substBasis [] _ = []
       substBasis _ [] = []
       substBasis ((t1, t2) : ts) ((Gama x tx) : gs)
                  | tx == t1 = (Gama x t2) : (substBasis ts gs)
                  | otherwise = (Gama x tx) : (substBasis ((t1, t2) : ts) gs)


       substType :: [(Type, Type)] -> Type -> Type
       substType [] t = t
       substType ((t1, t2) : ts) alpha = if t1 == alpha then t2 else (substType ts alpha)

       makePair :: [String] -> [Basis] -> [Basis] -> [(Type, Type)]
       makePair [] _ _ = []
       makePair (x : xs) ((Gama y1 t1) : g1) ((Gama y2 t2) : g2)
                | (searchBasis ((Gama y1 t1) : g1) x) == True && (searchBasis ((Gama y2 t2) : g2) x) == True = (getType x ((Gama y1 t1) : g1),  getType x ((Gama y2 t2) : g2)) : (makePair xs ((Gama y1 t1) : g1) ((Gama y2 t2) : g2))
                | otherwise = (makePair xs ((Gama y1 t1) : g1) ((Gama y2 t2) : g2)) 

       milner :: Expr -> Int -> Char -> ([Basis], Type, Int, Char)
       milner (VarE x) oc s = ([Gama x (VarT (s : show oc))], (VarT (s : show oc)), oc, s)
       milner (Lambda x expr) oc s =
                                 let a = getType x g
                                     (g, t, c, s1) = (milner expr (count oc) s)
                                 in if (searchBasis g x) == False then (g, (AppT (VarT (s : show oc)) t), oc, s) else ((remove g x), (AppT a t), oc, s)
       milner (AppE m1 m2) oc s =
                               let v = intersect (freeVar m1) (freeVar m2)
                                   ss = unify ((makePair v g1 g2) ++ [(t1, AppT t2 (VarT (s : (show oc))))]) (s, oc)
                                   (g1, t1, c1, a1) = (milner m1 (count oc) 'a')
                                   (g2, t2, c2, a2) = (milner m2 (count oc) 'b')
                               in ((substBasis ss g1 ++ g2), (substType ss (VarT (s : (show oc)))), oc, s)

--Examples

       ex1 :: Expr
       ex1 = Lambda "x" (Lambda "y" (Lambda "z" (AppE (AppE (VarE "x") (VarE "z")) (VarE "y"))))

       ex2 :: Expr
       ex2 = Lambda "x" (Lambda "y" (AppE (Lambda "z" (VarE "x")) (AppE (VarE "y") (VarE "x"))))

       ex3 :: Expr
       ex3 = Lambda "x" (Lambda "y" (AppE (VarE "y") (AppE (Lambda "z" (AppE (AppE (VarE "z") (VarE "x")) (VarE "x"))) (VarE "x"))))

       ex4 :: Expr
       ex4 = Lambda "x" (Lambda "y" (AppE (AppE (VarE "x") (VarE "y")) (Lambda "z" (AppE (VarE "y") (VarE "z")))))

       ex5 :: Expr
       ex5 = Lambda "x" (AppE (VarE "y") (VarE "z"))