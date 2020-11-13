module Typeinf where
       import TypeinfData

       unify :: ([Set], Int) -> ([Set], Int)
       unify ([], oc) = ([], oc)
       unify (((Equal t1 t2) : s), oc) = if t1 == t2 then unify (s, oc) else unify (((Equal t1 t2) : s), oc)
       unify (((Equal (AppT t1 t2) (AppT t3 t4)) : s), oc) = unify (((Equal t1 t3) : (Equal t2 t4) : s), oc)
       unify (((Equal t1 t2) : s), oc) = if t2 == (VarT ('a' : show oc)) then unify (((Equal t2 t1) : s), oc) else (((Equal t1 t2) : s), oc)
       

       algorithmT :: Expr -> Int -> ([Basis], Type, Int)
       algorithmT (VarE x) oc = ([Gama x (VarT ('a' : show oc))], (VarT ('a' : show oc)), oc)
       algorithmT (Lambda x expr) oc = if (search g x) == False then (g, (AppT (VarT ('a' : show oc)) t), oc) else ((remove g x), AppT (VarT "t") (VarT ('a' : show oc)), oc)
                                     where (g, t, c) = (algorithmT expr (count oc))
{-       algorithmT (AppE m1 m2) oc = (unify ((turnSet (getType g1 v) (getType g2 v)), oc), unify ((Equal t1 (AppT t2 (VarT 'a' : (show oc)))), oc) 
                                  where v = intersect (freeVar m1) (freeVar m2)
				        (g1, t1, c1) = (algorithmT m1 (count oc))
				        (g2, t2, c2) = (algorithmT m2 (count oc))

-}












