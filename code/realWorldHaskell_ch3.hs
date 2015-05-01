module CH3 where

import Data.List (sortBy)


	

myLength1 :: [a] -> Int
myLength1 []  	= 0
myLength1 (x:xs)= 1 + myLength1 xs

myLength2 :: [a] -> Int
myLength2 = foldl (\acc x -> acc+1) 0


mean [] = 0
mean xs	= let sumL  = sum xs
              lngt = myLength2 xs
	  in 	 sumL / (fromIntegral lngt)


turnPal xs = xs ++ (reverse xs)


turnPal2 [] = []
turnPal2 (x:xs) = x : turnPal2 xs ++ [x]


isPal ::(Eq a)=> [a] -> Bool
isPal [] = True
isPal (x:[]) = True
isPal (x:xs)
	| x /= last xs = False
	| otherwise = isPal (init xs)

sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy (\x y -> length x `compare` length y)

--interperse clone
joinLists :: a -> [[a]] -> [a]
joinLists _ [] = []
joinLists _ (x:[]) = x
joinLists s (x:xs) = x ++ [s] ++ (joinLists s xs)


data Tree a = Node a (Tree a) (Tree a)
	    | Leaf	
	deriving (Show)	

height :: Tree a -> Int
height Leaf   = 0
height (Node v tl tr) = let maxh = max (height tl) (height tr) 
			in 1 + maxh

data Direction = Right | Left | Straight deriving (Show)	

