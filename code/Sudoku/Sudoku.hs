module Sudoku where

import Data.Char(intToDigit, digitToInt)
import Data.List ((\\),sortBy,intercalate)
import Control.Monad (forM)
import Data.Maybe (isJust)
import Control.DeepSeq


data Board= Board [Square]
	deriving(Show)

--DO A n*n solver
--solve eurler problem96

-- A Square contains its column (ColDigit), row (RowDigit), and
-- which 3x3 box it belongs to (BoxDigit).  The box can be computed
-- from the row and column, but is kept for speed.
-- A Square also contains it's status: either a list of possible
-- digits that can be placed in the square OR a fixed digit (i.e.,
-- the square was given by a clue or has been solved).
data Square= Square ColDigit RowDigit BoxDigit (Either [Digit] Digit)
	deriving (Show)

type RowDigit= Digit
type ColDigit= Digit
type BoxDigit= Digit
type Digit= Char 

initBoard :: Board
initBoard = Board [Square col row (boxDigit col row) (Left allDigits) |  col<-allDigits,row<-allDigits]

allDigits :: [Char]
allDigits = ['1'..'9']

allDigits_zero :: [Char]
allDigits_zero = ['0'..'9']



--from 2D coordinate to boxIndex 
boxDigit :: ColDigit-> RowDigit -> BoxDigit
boxDigit col row = let linearize (r ,c) = intToDigit (r*3+c)
			in linearize ( (digitToInt row -1) `div` 3, (digitToInt col -1)`div` 3)
	 

getSolvedBoard :: Board -> [[Digit]]
getSolvedBoard sdk = [getRow sdk r | r<-allDigits]

getRow :: Board ->RowDigit->[Digit]
getRow (Board sdk) row = [getDigit d | Square _ row' _ d <-sdk , row'==row]
 where 
	getDigit (Left _) = '0'
	getDigit (Right v) = v


mkSquare :: ColDigit->RowDigit->Digit->Square
mkSquare c r d 
	| (legalDigit r && legalDigit c && legalDigit d) = Square c r (boxDigit c r) (Right d)
	| otherwise = error "Unvalid mkquare"
		where
			legalDigit dd = dd `elem` allDigits



-- Place a given Square on a Board and return the new Board.
-- Take care of removing the same (Right d) from same row and col and box
-- Illegal setSquare calls will just error out(one with Left  in the maybe Either part).
-- The main work here is to remove the placed digit from the other Squares on the board
-- that are in the same column, row, or box.
setSquare :: Square-> Board ->Board
setSquare sq@(Square scol srow sbox (Right d))(Board sqs) = Board (map set sqs)
	where
		set osq@(Square ocol orow obox ods)
			| scol==ocol && srow==orow  			 = sq
			| scol==ocol || srow==orow || sbox==obox = Square ocol orow obox (checkEither ods)
			| otherwise 							 = osq
		
		checkEither (Left  lds ) 			= Left (lds \\ [d])
		checkEither r@(Right d'   ) | d==d'	= error "Already set this square"
		checkEither dd = dd
setSquare _ _ = error "Bad call to setSquare"


getUnsolvedSquare :: Board -> [Square]
getUnsolvedSquare (Board sqs) = filter (isSolved) sqs
	where 
		isSolved (Square _ _ _ (Left _)) = True
		isSolved _						  = False


-- Given an initial Board return all the possible solutions starting
-- from that Board.
-- Note, this all happens in the list monad and makes use of lazy evaluation
-- to avoid work.  Using the list monad automatically handles all the backtracking
-- and enumeration of solutions.
solveMany :: Board -> [Board]
solveMany brd = 
	case getUnsolvedSquare brd of
		[] 	-> return brd --all squares correctly set up
		sqs -> do 
		let Square c r b (Left ds) : _ = sortBy (byLeftLenght) sqs
		sq <- [ Square c r b (Right d) | d <- ds ] -- Try all possible moves
		solveMany (setSquare sq brd)
			
		
byLeftLenght :: Square->Square->Ordering
byLeftLenght (Square _ _ _ (Left xs)) (Square _ _ _ (Left ys)) = compare (length xs) (length ys)
byLeftLenght _ _ = error "Bad byLeftLength"


		
solve :: String -> Maybe Board
solve s = case solveMany (readBoard s) of
		[] -> Nothing
		b:_ -> Just b
		
	
-- Return a list of rows of a solved board.
-- If used on an unsolved board the return value is unspecified.
getBoard :: Board -> [[Char]]
getBoard (Board sqs) = [ [ getDigit d | Square _ row' _ d <- sqs, row' == row ] | row <- allDigits ]
  where getDigit (Right d) = d
        getDigit _ = '0'



--if the input is correct -> 81 digits between 0 and 9!
readBoard :: String -> Board
readBoard sdkstr 
	| length sdkstr == 81 {-&& (foldr1 (&&) (map (`elem` allDigits_zero) sdkstr))==True -}
												= (makeSquareList initBoard sdkstr 1 1)
	| otherwise = error "Malformed Input"


makeSquareList:: Board -> String->Int ->Int->Board
makeSquareList brd [] _ _ 			  = brd 
makeSquareList brd (dig:digs) col row = makeSquareList brd' digs (fst ncoord)  (snd ncoord)
			where 
				brd' 	= 	if 	 (dig /= '0')
							then setSquare (mkSquare (intToDigit col) (intToDigit row) dig) brd
							else brd			
				ncoord	= 	if (col < 9)
							then (col+1,row)
							else (1,row+1)
