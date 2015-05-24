{-# LANGUAGE FlexibleInstances,OverlappingInstances #-}
module JSON where

import Data.List (intercalate)
import Control.Arrow (second)

newtype JAry a = JAry {
		fromJAry :: [a]
	} deriving (Eq,Ord,Show)

newtype JObj a = JObj {
		fromJObj :: [(String, a)]
	} deriving (Eq,Show,Ord)

data JValue = JString String
	| JNumber Double
	| JBool Bool
	| JNull
	| JObject (JObj JValue)
	| JArray (JAry JValue)
	 deriving (Eq,Ord,Show)
	
type JSON_ERROR = String

class JSON a where
	toJValue :: a -> JValue
	fromJValue :: JValue -> Either JSON_ERROR a



instance JSON JValue where
	toJValue   = id
	fromJValue = Right 

instance JSON Bool where
	toJValue   = JBool
	fromJValue (JBool b) = Right b
	fromJValue _ = Left "Not a JSON Bool"
 
jNumberToNumber :: (Double -> a) -> JValue -> Either JSON_ERROR a
jNumberToNumber f (JNumber v) 	= Right (f v)
jNumberToNumber _ _ 		= Left "Not a JNumber Value"

instance JSON Int where
	toJValue   	= JNumber . fromIntegral
	fromJValue	= jNumberToNumber round

instance JSON Integer where
	toJValue   	= JNumber  . fromIntegral
	fromJValue	= jNumberToNumber round

instance JSON Double where
	toJValue   	= JNumber 
	fromJValue	= jNumberToNumber id


instance JSON String where
	toJValue	=JString
	fromJValue (JString s) = Right s
	fromJValue _ = Left "Not  Jstring Value"


jaryToJValue :: (JSON a) => (JAry a) -> JValue
jaryToJValue = JArray . JAry . map toJValue . fromJAry

jaryFromJValue :: (JSON a) => JValue -> Either JSON_ERROR (JAry a)
jaryFromJValue (JArray (JAry a)) =
    whenRight JAry (mapM fromJValue a)
jaryFromJValue _ = Left "not a JSON array"

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a) = Right (f a)

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                        Left err -> Left err
                        Right ys -> case f x of
                                      Left err -> Left err
                                      Right y -> Right (y:ys)
mapEithers _ _ = Right []

instance (JSON a) => JSON (JAry a) where 
	toJValue 	= jaryToJValue --JArray (foldr ((:) . toJValue) [] v)
	fromJValue 	= jaryFromJValue --mapM (fromJValue) v

instance (JSON a) => JSON (JObj a) where 
	toJValue 	= JObject . JObj . map (second toJValue) . fromJObj
	fromJValue (JObject (JObj o))	=  whenRight JObj (mapM f o)
		where f (k,v) = whenRight ((,) k) (fromJValue v)
	fromJValue _			=  Left "Not a JSON OBJECT"


listToJValue :: (JSON a ) => [a] -> [JValue]
listToJValue = map (toJValue)

jvaluesToJAry :: [JValue] -> JAry JValue
jvaluesToJAry = JAry

jaryOfJValuesToJValue :: JAry JValue -> JValue
jaryOfJValuesToJValue = JArray
{-
instance (JSON a) => JSON [(String,a)] where 
	toJValue 	= undefined
	fromJValue	= undefined
-}

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

{-
renderJValue_naive :: JValue -> String
renderJValue_naive (JString s) = s
renderJValue_naive (JNumber n) = show n
renderJValue_naive (JBool b)	 = show b
renderJValue_naive JNull	 = "null"
renderJValue_naive (JAry a) = "[" ++ (intercalate "," (map renderJValue_naive a)) ++"]"
--renderJValue_naive (JObj a) = "[" ++ (intercalate "," (map showPair a)) ++"]"
	--where showPair (s,v) =  s++":"++ renderJValue_naive v


putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue_naive v)
-}
