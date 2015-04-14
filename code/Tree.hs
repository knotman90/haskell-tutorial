module Tree where


data Tree a = Node a (Tree a) (Tree a) | Leaf

instance (Show a)=> Show (Tree a) where
	show Leaf = "Leaf"
	show (Node e l r) = "("++show e++" "++show l++" "++show r++")"

instance (Eq a) =>Eq (Tree a) where
 (==) Leaf Leaf = True
 (==) (Node e l r) (Node e1 l1 r1) = (e==e1) && l==l1 && r==r1
 (==) _ _  = False

instance (Ord a) => Ord (Tree a) where
 compare Leaf Leaf = EQ
 compare Leaf (Node _ _ _) = LT
 compare (Node _ _ _) Leaf = GT
 compare (Node e l r) (Node e1 l1 r1) 
	|e > e1 	= GT
	|e < e1 	= LT
	|otherwise  = if cll1 /= EQ then cll1 else crr2
	where 	
			cll1 = compare l l1
			crr2 = compare r r1  
