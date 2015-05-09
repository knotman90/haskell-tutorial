module Ch4 where
import Data.Char (digitToInt, isDigit)


myGroupBy_fold :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy_fold p  = foldr step []
	where 
		step x [] = [[x]]
		step x l@(aa@(a:_):ls) = if (p x a) then ((x:aa)):ls else [x]:l

--let p = (==) in quickCheck (\s ->myGroupBy p s == groupBy p s) --this to check that the function axtually is correct!
myGroupBy :: (a-> a-> Bool) -> [a] -> [[a]]
myGroupBy p l     = myGroupBy' p [] l      
	where 
	       myGroupBy' p acc [] = acc
	       myGroupBy' p [] (x:xs) =  myGroupBy' p ([[x]]) xs
	       myGroupBy' p aa@(a:as) (x:xs) =  let newAcc =  if (p (head a) x) then  ((x:a):as) else [x]:aa
					        in  myGroupBy' p newAcc xs


--write ytour own definition of takeWhile first using explicit recursion then using foldr
myTakeWhile_rec :: (a -> Bool) -> [a] -> [a]
myTakeWhile_rec p l= myTakeWhile_rec' p [] l
	where
		myTakeWhile_rec' p acc [] =  acc
		myTakeWhile_rec' p acc (x:xs) = if(p x) 
						then myTakeWhile_rec' p (acc++[x]) xs 
						else acc

myTakeWhile_foldl :: (a -> Bool) -> [a] -> [a]
myTakeWhile_foldl p l = snd $ foldl step (True,[]) l
	where 
		step  (True,acc)  x = if (p x) then (True,acc++[x]) else (False,acc)
		step  (False,acc) _ = (False,acc)	


takeWhile_foldr :: (a -> Bool) -> [a] -> [a]
takeWhile_foldr p = foldr (\x xs -> if p x then x:xs else []) []


concat_foldr :: [[a]] ->[a]
concat_foldr  = foldr (++) []

--synonim for String
type ErrorMessage = String

--wrapper for different asIntfold
asInt :: String -> Either ErrorMessage Int
--asInt :: String -> Int
asInt l@(x:xs) 
	|x=='-' 	= let n=foldFunction xs
			  in 	
				case n of
				  (Left x) -> (Left x)
				  (Right x) -> (Right (negate x))	
	|otherwise 	= foldFunction l
	where foldFunction = asInt_either

asInt_either :: String -> Either ErrorMessage Int
asInt_either = foldl step (Right 0)	
	where 
	 step(Left a) _     = Left a
	 step (Right acc) x = let next = acc*10+ (digitToInt x)
			   in if (isDigit x) then (Right next) else (Left "Invalid Char")

asIntFold :: String -> Int
asIntFold = foldl step 0	
	where step acc x = acc*10+ (digitToInt x)
			    


asIntFold_error :: String -> Int
asIntFold_error = foldl step 0	
	where step acc x = let next = acc*10+ (digitToInt x)
			   in if (isDigit x) then next else error "Invalid char"
