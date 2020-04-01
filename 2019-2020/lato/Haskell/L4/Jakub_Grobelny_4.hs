-- Jakub Grobelny
-- Kurs języka Haskell
-- Lista 4, 3.04.2020

-- Zadanie 1

data BTree a
    = BNode (BTree a) a (BTree a)
    | BLeaf
  deriving Show

flatten :: BTree a -> [a]
flatten = flatten' []
  where
    flatten' acc BLeaf = acc
    flatten' acc (BNode l v r) = flatten' (v:rs) l
      where
        rs = flatten' acc r

qsort :: Ord a => [a] -> [a]
qsort = qsort' []
  where
    qsort' acc []  = acc
    qsort' acc (pivot:xs) = qsort' (pivot:rs) [x | x <- xs, x < pivot]
      where
        rs = qsort' acc [x | x <- xs, x >= pivot]