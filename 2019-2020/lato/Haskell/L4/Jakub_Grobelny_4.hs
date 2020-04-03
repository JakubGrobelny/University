-- Jakub Grobelny
-- Kurs języka Haskell
-- Lista 4, 3.04.2020

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

-- Zadanie 2

queens :: Int -> [[Int]]
queens maxN = queens' maxN
  where
    queens' 0 = [[]]
    queens' n = do
        qs <- queens' (n - 1)
        q  <- [1 .. maxN]
        if safe q qs 1
            then return (q:qs)
            else []
    safe _ [] _ = True
    safe q' (q:qs) col = q' /= q
                      && abs (q - q') /= col
                      && safe q' qs (col + 1)

--------------------------------------------------------------------------------

data BinTree
    = BinTreeLeaf
    | BinTree :/\: BinTree
  deriving Show

-- Zadanie 3    

binTree :: Int -> BinTree
binTree 0 = BinTreeLeaf
binTree n
    -- sprawdzamy, czy n jest parzyste. Jeżeli jest, to wówczcas create2
    -- nie utworzy zbędnego drzewa i wystarczy zlączyć wyniki.
    | even n    = uncurry (:/\:) $ create2 ((n - 1) `div` 2)
    -- jeżeli n było nieparzyste, to wówczas jedno z drzew zwracanych z create2
    -- byłoby nieużytkiem. Wystarczy więc zamiast tego policzyć jedno drzewo
    -- o rozmiarze (n `div` 2) i użyć go do zbudowania drzewa o rozmiarze n
    | otherwise = let t = binTree (n `div` 2) in t :/\: t
  where
    even = (== 0) . (`mod` 2)
    create2 :: Int -> (BinTree, BinTree)
    create2 0 = (BinTreeLeaf, BinTreeLeaf :/\: BinTreeLeaf)
    create2 m
        | even m    = (t1 :/\: t2, t2 :/\: t2)
        | otherwise = (t1 :/\: t1, t1 :/\: t2)
      where
        (t1, t2) = create2 $ (m - 1) `div` 2

--------------------------------------------------------------------------------

class Queue q where
    emptyQ   :: q a
    isEmptyQ :: q a -> Bool 
    put      :: a -> q a -> q a
    get      :: q a -> (a, q a)
    top      :: q a -> a
    pop      :: q a -> q a
    get q = (top q, pop q)
    top = fst . get
    pop = snd . get

-- Zadanie 5

data SimpleQueue a = SimpleQueue
    { front :: [a]
    , rear  :: [a]
    }
  deriving Show

instance Queue SimpleQueue where
    emptyQ = SimpleQueue [] []
    
    isEmptyQ (SimpleQueue [] []) = True
    isEmptyQ _ = False

    put x q = q { rear = x : rear q }

    get q@(SimpleQueue (x:xs) _) = (x, q { front = xs })
    get (SimpleQueue [] rear)    = get (SimpleQueue (reverse rear) [])