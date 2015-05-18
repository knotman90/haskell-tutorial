module Ch4 where
import Data.Char (digitToInt, isDigit,isUpper)



--example count the number of word in a string that satisfy a predicate p
numElmSatisfy :: (a->Bool)-> [a] -> Int
numElmSatisfy p = (length . (filter p)) 

--numWordsSatisfy :: (a->Bool)-> [a] -> Int
numWordsSatisfy p = (length . (filter p) . words) 

numWordsStartsUpper = let p = (isUpper . head) in numWordsSatisfy p

--function composition (.) in Prelude
compose :: (b->c) -> (a->b) -> a -> c
compose f g a =  f (g a)


--does not produce the null element at the end of the list also implementable as 
myTails'' = init . myTails

myTails' :: [a] -> [[a]]
myTails' ys@(_:xs) = ys :  myTails' xs
myTails' _	= []


myTails :: [a] -> [[a]]
myTails [] 	= [[]]
myTails xs 	= let tal = (tail xs)in [xs]++ (myTails tal)
 
{-
How many of the following Prelude functions can you rewrite using list folds?
For those functions where you can use either foldl' or foldr, which is more appropriate in each case?
-}

--unlines------------------------------------------------------- 
myUnlines :: [String] -> String
myUnlines = foldr (\x acc -> x++"\n"++acc) ""

--words ------------------------------------------------------- 
myWords :: String -> [String]
myWords = foldr step []
	where
	step x acc@(y:ys) = if (x=='\n') then ([]:acc) else ((x:y):ys)
	step x acc =  [[x]]
		
--cycle ------------------------------------------------------- 
infList :: [Int]
infList=repeat 1

myCycle :: [a] -> [a]
myCycle  [] =  error "Empty lists"
myCycle  xs =  xs ++ (myCycle xs)

myCycle_foldr :: [a] -> [a]
myCycle_foldr [] = error "Empty Lists"
myCycle_foldr  xs  = foldr (\_ acc -> xs++acc) [] (infList) --[1..]


--any -------------------------------------------------------
my_any :: (a->Bool) -> [a] -> Bool
my_any _ [] = False
my_any p (x:xs) 
	| p x 	    = True
	| otherwise = my_any p xs

--let p = (even) in quickCheck (\s ->any p s == my_any_fold p s)
my_any_fold :: (a->Bool) -> [a] -> Bool
my_any_fold p = foldr (\x acc-> acc || p x) False




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
