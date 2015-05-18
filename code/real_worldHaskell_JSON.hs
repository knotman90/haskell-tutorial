module JSON where


import Data.List (intercalate)

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


renderJValue_naive :: JValue -> String
renderJValue_naive (JString s) = s
renderJValue_naive (JNumber n) = show n
renderJValue_naive (JBool b)	 = show b
renderJValue_naive JNull	 = "null"
renderJValue_naive (JArray a) = "[" ++ (intercalate "," (map renderJValue_naive a)) ++"]"
renderJValue_naive (JObject a) = "[" ++ (intercalate "," (map showPair a)) ++"]"
	where showPair (s,v) =  s++":"++ renderJValue_naive v


putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue_naive v)
