import Control.Monad
import Data.List

file="sudokus.txt"

main = do
	str<-readFile file
	let fileLines =filter (\l -> length l > 0) (lines str)
	print fileLines
	let sudokus = createSudokuList fileLines
	writeBoardFile sudokus 0
	print "END"



writeBoardFile (sdk:sds) i= do
			writeFile ("sudoku_"++(show i)) sdk
			writeBoardFile sds (i+1)
writeBoardFile  _ _ = return ()			


createSudokuList fl 
	|length fl <=  9 = []
	|otherwise 		= (concat (take 9 fl)) : createSudokuList (drop 9 fl)
	
