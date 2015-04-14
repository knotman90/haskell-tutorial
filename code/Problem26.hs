module Main where

import Data.List
{-

A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

1/2	= 	0.5
1/3	= 	0.(3)
1/4	= 	0.25
1/5	= 	0.2
1/6	= 	0.1(6)
1/7	= 	0.(142857)
1/8	= 	0.125
1/9	= 	0.(1)
1/10	= 	0.1
Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

 The length of the repetend (period of the repeating decimal) of 1/p is equal to the order of 10 modulo p. If 10 is a primitive root modulo p, the repetend length is equal to p − 1; if not, the repetend length is a factor of p − 1. This result can be deduced from Fermat's little theorem, which states that 10p−1 = 1 (mod p).
-}
--modulo, current order
order :: Integer -> Integer -> Integer
order a ord 
	| mod (10^ord) a == 1 = ord
	| ord > a 			  = 0
	| otherwise 		  = order a (ord+1) 


maxo = (fst . last) $ sortBy (\x y -> (snd x) `compare` (snd y)) $ map (\x->(x,order x 1)) [1..1000]


main = do
	putStrLn ("The solution is: " ++(show maxo) )
