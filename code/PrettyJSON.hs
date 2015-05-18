module JSON where


import Data.List (intercalate)
import Prettify

data JValue = JString String
	| JNumber Double
	| JBool Bool
	| JNull
	| JObject [(String,JValue)]
	|JArray [JValue]
	 deriving (Eq,Ord,Show)
	

getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool (JBool b) = Just b
getBool _         = Nothing

getObject (JObject o) = Just o
getObject _           = Nothing

getArray (JArray a) = Just a
getArray _          = Nothing

isNull v            = v == JNull


renderJValue :: JValue -> Doc
renderJValue (JString s) = string s
renderJValue (JNumber n) = string $ show n
renderJValue (JBool b)	 = string $ show b 
renderJValue JNull	 = text "null"
renderJValue (JArray a)	 = series '[' ']' renderJValue a
renderJValue (JObject o)	 = series '{' '}' (\(s,v)->string s <> text ":" <> renderJValue v) o


--putJValue :: JValue -> IO ()
--putJValue v = putStrLn (renderJValue v)
