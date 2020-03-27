-- Jakub Grobelny
-- Kurs języka Haskell
-- Lista 3, 27.03.2020

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}

data BTree t a
    = Node (t a) a (t a)
    | Leaf

class BT t where
    toTree :: t a -> BTree t a

data UTree a 
    = UNode (UTree a) a (UTree a) 
    | ULeaf

instance BT UTree where
    toTree ULeaf = Leaf
    toTree (UNode l x r) = Node l x r

newtype Unbalanced a = Unbalanced { fromUnbalanced :: BTree Unbalanced a }

instance BT Unbalanced where
    toTree = fromUnbalanced

--------------------------------------------------------------------------------

-- Zadanie 1

treeSize :: BT t => t a -> Int
treeSize (toTree -> Leaf) = 0
treeSize (toTree -> Node l _ r) = 1 + treeSize l + treeSize r

treeLabels :: BT t => t a -> [a]
treeLabels (toTree -> Leaf) = []
treeLabels (toTree -> Node l a r) = treeLabels l ++ [a] ++ treeLabels r

treeFold :: BT t => (b -> a -> b -> b) -> b -> t a -> b
treeFold _ n (toTree -> Leaf) = n
treeFold f n (toTree -> Node l a r) = f (treeFold f n l) a (treeFold f n r)

--------------------------------------------------------------------------------

-- Zadanie 2

searchBT :: (Ord a, BT t) => a -> t a -> Maybe a
searchBT _ (toTree -> Leaf) = Nothing
searchBT val (toTree -> Node l a r)
    | val < a   = searchBT val l
    | val > a   = searchBT val r
    | otherwise = Just a

toUTree :: BT t => t a -> UTree a
toUTree (toTree -> Leaf) = ULeaf
toUTree (toTree -> Node l a r) = UNode (toUTree l) a (toUTree r)

toUnbalanced :: BT t => t a -> Unbalanced a
toUnbalanced (toTree -> Leaf) = Unbalanced Leaf
toUnbalanced (toTree -> Node l a r) = Unbalanced $ 
    Node (toUnbalanced l) a (toUnbalanced r)

--------------------------------------------------------------------------------

-- Zadanie 3

instance (BT t, Show a) => Show (t a) where
    show (toTree -> Leaf) = "-"
    show (toTree -> Node l a r) = show' l ++ " " ++ show a ++ " " ++ show' r
      where
        show' leaf@(toTree -> Leaf) = show leaf
        show' tree = "(" ++ show tree ++ ")"

--------------------------------------------------------------------------------

class BT t => BST t where
    node :: t a -> a -> t a -> t a
    leaf :: t a

instance BST UTree where
    node = UNode
    leaf = ULeaf

instance BST Unbalanced where
    node l x r = Unbalanced $ Node l x r
    leaf = Unbalanced Leaf

class Set s where
    empty  :: s a
    search :: Ord a => a -> s a -> Maybe a
    insert :: Ord a => a -> s a -> s a
    delMax :: Ord a => s a -> Maybe (a, s a)
    delete :: Ord a => a -> s a -> s a

--------------------------------------------------------------------------------

-- Zadanie 6

instance BST s => Set s where
    empty  = leaf
    search = searchBT
    
    insert a (toTree -> Leaf) = node leaf a leaf
    insert a tree@(toTree -> Node l v r)
        | a < v     = node (insert a l) v r
        | a > v     = node l v (insert a r)
        | otherwise = tree

    delMax (toTree -> Leaf) = Nothing
    delMax (toTree -> Node l v (toTree -> Leaf)) = Just (v, l)
    delMax (toTree -> Node l v r) = do
        (max, r') <- delMax r
        return (max, node l v r')

    delete a (toTree -> Leaf) = leaf
    delete a (toTree -> Node l v r)
        | a < v     = node (delete a l) v r
        | a > v     = node l v (delete a r)
        | otherwise = case delMax l of
            Just (max, l) -> node l max r
            Nothing       -> r

