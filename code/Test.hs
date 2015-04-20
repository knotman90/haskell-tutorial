module Main where
div' :: (Fractional a,Eq a) => a-> a-> Maybe a
div' a b = if b==0 then Nothing else Just (a/b)


cE a b xs = case (a `compare` b,xs) of
	(_,[]) -> []
	(LT,xs) -> init xs
	(GT,xs) -> tail xs 
	(EQ,xs) -> [head xs]
