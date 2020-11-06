module Typeinf where
       import TypeinfData

       algorithmT :: Expr -> ([Basis], Type)
       algorithmT (VarE x) = ([Gama x (VarT "alpha")], VarT "alpha")
       algorithmT (Lambda x expr t) = if (search g x) == False then (g, AppT (VarT "beta") t2) else ((remove g x t), AppT t t2)
                                 where (g, t2) = (algorithmT expr)
      -- algorithmT (AppE e1 e2) = 