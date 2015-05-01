module Main where

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (maximumBy)

maxDay ::String -> String
maxDay s = snd $ maximum  ss
	where ss = map (var . (splitOn ",")) $ lines s

var ::[String] -> (Double,String)
var xs = (abs (read (xs !! 1)) - (read (last xs)), head xs)

main = do
	(fileName:_) <- getArgs 
	strF <- readFile fileName
	putStrLn $ maxDay strF	

