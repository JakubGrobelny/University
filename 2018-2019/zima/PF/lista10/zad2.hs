data BTree a = Leaf | Node (BTree a) a (BTree a) deriving (Eq, Ord, Show)

data Array a = Arr (BTree a) Integer deriving Show

aempty :: Array a
aempty = Arr Leaf 0

asub :: Array a -> Integer -> a
asub (Arr t _) i = find t i
    where
        find :: BTree a -> Integer -> a
        find Leaf _ = error "Index out of bounds!"
        find (Node l v r) index
            | index == 1 = v
            | index `mod` 2 == 0 =
                find l $ index `div` 2
            | otherwise =
                find r $ index `div` 2

aupdate :: Array a -> Integer -> a -> Array a
aupdate (Arr t size) i a = Arr (change t i a) size
    where
        change :: BTree a -> Integer -> a -> BTree a
        change Leaf _ _ = error "Index out of bounds!"
        change (Node l v r) i a
            | i == 1 = Node l a r
            | i `mod` 2 == 0 =
                Node (change l (i `div` 2) a) v r
            | otherwise =
                Node l v (change r (i `div` 2) a)

ahiext :: Array a -> a -> Array a
ahiext (Arr Leaf _) a = Arr (Node Leaf a Leaf) 1
ahiext (Arr t size) a = Arr (insert t a $ size + 1) $ size + 1
    where
        insert :: BTree a -> a -> Integer -> BTree a
        insert Leaf a _ = Node Leaf a Leaf
        insert (Node l v r) a k
            | k `mod` 2 == 0 =
                Node (insert l a $ k `div` 2) v r
            | otherwise =
                Node l v (insert r a $ k `div` 2)

ahirem :: Array a -> Array a
ahirem (Arr t size) = Arr (remove t size) $ size - 1
    where
        remove :: BTree a -> Integer -> BTree a
        remove Leaf _ = error "Cannot remove element from an empty array!"
        remove (Node l v r) i
            | i == 1 = Leaf
            | i `mod` 2 == 0 =
                Node (remove l $ i `div` 2) v r
            | otherwise = 
                Node l v (remove r $ i `div` 2)

-- funkcja pomocnicza do wprowadzania tablic
listToArray :: [a] -> Array a
listToArray xs = listToArray' xs aempty
    where
        listToArray' :: [a] -> Array a -> Array a
        listToArray' [] acc = acc
        listToArray' (x:xs) acc =
            listToArray' xs (ahiext acc x)
