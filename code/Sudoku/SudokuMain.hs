import Control.Parallel.Strategies
import System.Environment (getArgs)
import Data.Maybe (isJust)
import Sudoku

main:: IO()
main = do
	[file] <-getArgs
	str<-readFile file
	let sudokus = (lines str)
	--let solutions = runEval (myParMap solve sudokus)
	let solutions = (map solve sudokus)
	let howManyHaveSol = length (filter(isJust) solutions)
	print $ (show (length sudokus))++":"++ (show howManyHaveSol)
	
			
myParMap :: (a->b) -> [a] ->Eval [b]
myParMap _ [] 	= return []
myParMap f (x:xs) = do 
	a<- rpar (f x)
	as<- myParMap f xs
	return (a:as)
		

	
