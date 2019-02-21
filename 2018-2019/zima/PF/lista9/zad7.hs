data Tree a = Node (Tree a) a (Tree a) | Leaf

data Set a = Fin (Tree a) | Cofin (Tree a)

-- Pomocnicze funkcje na drzewach
insert :: Ord a => a -> Tree a -> Tree a
insert a Leaf = Node Leaf a Leaf
insert a (Node left val right)
    | a < val = Node (insert a left) val right
    | a == val = Node left val right
    | otherwise = Node left val (insert a right)

buildTree :: Ord a => [a] -> Tree a
buildTree [] = Leaf
buildTree (x:xs) = insert x $ buildTree xs

mergeTrees :: Ord a => Tree a -> Tree a -> Tree a
mergeTrees Leaf Leaf = Leaf
mergeTrees Leaf t = t
mergeTrees t Leaf = t
mergeTrees t (Node l v r) = insert v (mergeTrees (mergeTrees t l) r)

isTreeElement :: Ord a => a -> Tree a -> Bool
isTreeElement _ Leaf = False
isTreeElement a (Node l v r)
    | a == v = True
    | a < v = isTreeElement a l
    | otherwise = isTreeElement a r

substractTrees :: Ord a => Tree a -> Tree a -> Tree a
substractTrees Leaf _ = Leaf
substractTrees t Leaf = t
substractTrees (Node l v r) t
    | isTreeElement v t = mergeTrees l0 r0
    | otherwise = insert v $ mergeTrees l0 r0
    where 
        l0 = substractTrees l t
        r0 = substractTrees r t

-- Pomocnicze funkcje do debugowania
treeToList :: Ord a => Tree a -> [a]
treeToList Leaf = []
treeToList (Node l v r) = treeToList l ++ [v] ++ treeToList r

setToTree :: Ord a => Set a -> Tree a
setToTree (Cofin t) = t
setToTree (Fin t) = t

setToList :: Ord a => Set a -> [a]
setToList s = (treeToList . setToTree) s

-- Funkcje z zadania
setFromList :: Ord a => [a] -> Set a
setFromList [] = Fin Leaf
setFromList xs = Fin (buildTree xs)

setEmpty :: Ord a => Set a
setEmpty = Fin Leaf

setFull :: Ord a => Set a
setFull = Cofin Leaf

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Fin s1) (Fin s2) = Fin $ mergeTrees s1 s2
setUnion (Cofin s1) (Cofin s2) = Cofin $ substractTrees whole diffs
    where 
        whole = mergeTrees s1 s2
        diffs = mergeTrees (substractTrees s1 s2) (substractTrees s2 s1)
setUnion (Fin fs) (Cofin cs) = Cofin $ substractTrees cs fs
setUnion (Cofin cs) (Fin fs) = setUnion (Fin fs) (Cofin cs)

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Fin s1) (Fin s2) = 
    setComplement $ setUnion (Cofin s1) (Cofin s2)
setIntersection (Cofin s1) (Cofin s2) = 
    setComplement $ setUnion (Fin s1) (Fin s2)
setIntersection (Fin fs) (Cofin cs) = setIntersection (Cofin cs) (Fin fs)
setIntersection (Cofin cs) (Fin fs) = Fin $ substractTrees fs cs

setComplement :: Ord a => Set a -> Set a
setComplement (Fin s) = Cofin s
setComplement (Cofin s) = Fin s

setMember :: Ord a => a -> Set a -> Bool
setMember _ (Fin Leaf) = False
setMember a (Cofin s) = not $ isTreeElement a s
setMember a (Fin t) = isTreeElement a t
