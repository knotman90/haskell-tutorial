module Main where
div' :: (Fractional a,Eq a) => a-> a-> Maybe a
div' a b = if b==0 then Nothing else Just (a/b)


cE a b xs = case (a `compare` b,xs) of
	(_,[]) -> []
	(LT,xs) -> init xs
	(GT,xs) -> tail xs 
	(EQ,xs) -> [head xs]



_last :: [a] -> a
_last [] 	 = error "Undefined operation"
_last (x:[]) = x
_last (x:xs) = _last xs


elementAt :: Integer -> [a] -> a
elementAt  _ [] 	 = error "index out of bound"
elementAt 1 (x:_)	 = x
elementAt n (_:xs)       = elementAt (n-1) xs 


palindrome1 l = l== reverse l
			
palindrome2 [] = True --empty list is palindrome
palindrome2 (_:[]) = True --one element is palindrome
palindrome2 l 
    | head l /= last l = False
    | otherwise 	= palindrome2 ll
     where
       ll = (tail . init) l  --(tail (init l) )

