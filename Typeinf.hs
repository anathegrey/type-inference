module Typeinf where
       import TypeinfData

       unify :: [(Type, Type)] -> String -> [(Type, Type)]
       unify (((AppT e1 e2), (AppT e3 e4)) : ts) s = (unify ((e1, e3) : (e2, e4) : ts) s)
       unify ((t1, t2) : ts) s
             | t1 == t2 = (unify ts s)
             | t2 == (VarT s) = (unify ((t2, t1) : ts) s) --falta o caso de alfa = t
             | otherwise = (unify (ts ++ [(t1, t2)]) s)
       
       milner :: Expr -> Int -> Char -> ([Basis], Type, Int, Char)
       milner (VarE x) oc s = ([Gama x (VarT (s : show oc))], (VarT (s : show oc)), oc, s)
       milner (Lambda x expr) oc s = if (search g x) == False then (g, (AppT (VarT (s : show oc)) t), oc, s) else ((remove g x), AppT (VarT "t") (VarT (s : show oc)), oc, s)
                                     where (g, t, c, s1) = (milner expr (count oc) s)
{-
       algorithmT (AppE m1 m2) oc = unify ((turnSet (getType g1 v) (getType g2 v)), (Equal t1 (AppT t2 a)), oc)
                                  where v = intersect (freeVar m1) (freeVar m2)
				        (g1, t1, c1) = (algorithmT m1 (count oc))
				        (g2, t2, c2) = (algorithmT m2 (count oc))
					a = (VarT ('a' : show oc))
-}










