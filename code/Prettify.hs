module Prettify where
 
import Numeric
import Data.Bits
import Data.Char

data Doc = Later

text :: String -> Doc
text = undefined

string :: String -> Doc
string = enclose '"' '"' . hcat .map oneChar

char :: Char -> Doc
char c = undefined

double :: Double -> Doc
double = undefined

series :: Char -> Char -> (a->Doc) -> [a] -> Doc
series left right item = enclose '"' '"' . fsep . punctuate (char ',') . map item

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = d<>p : punctuate p ds

fsep :: [Doc] -> Doc
fsep = undefined 


enclose :: Char -> Char -> Doc -> Doc
enclose left right d = char left <> d <> char right

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
		(Just r) -> text r
		(Nothing) | mustEscape c -> hexEscape c
			  | otherwise -> char c
	where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char,String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
	where ch a b = (a,['\\',b])

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
	    | otherwise = astral (d- 0x10000)
	where d = ord c

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff


smallHex :: Int -> Doc
smallHex x = text "\\u" <> text (replicate (4 -length h) '0') <> text h
	where h = showHex x ""
