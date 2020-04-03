-- Jakub Grobelny
-- Kurs języka Haskell
-- Lista 4, 3.04.2020

{-# LANGUAGE ViewPatterns #-}

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

-- Zadanie 4

binTreeLeaves :: Int -> BinTree
binTreeLeaves n = binTree (n - 1) BinTreeLeaf BinTreeLeaf
  where
    binTree :: Int -> BinTree -> BinTree -> BinTree
    binTree 0 acc t = acc
    binTree n acc t
        | n `mod` 2 == 1 = binTree n' (t :/\: acc) t'
        | otherwise      = binTree n' acc t'
      where
        t' = t :/\: t
        n' = n `div` 2

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

-- Zadanie 6

primes :: [Integer]
primes = 
    2 : [ p | p <- [3..], 
              and [ p `mod` q /= 0 | q <- takeWhile (\q -> q * q <= p) primes]]

--------------------------------------------------------------------------------

-- Zadanie 7

fib :: [Integer]
fib = 1 : 1 : zipWith (+) fib (tail fib)

--------------------------------------------------------------------------------

-- Zadanie 8

(<+>) :: Ord a => [a] -> [a] -> [a]
xs <+> [] = xs
[] <+> ys = ys
xs'@(x:xs) <+> ys'@(y:ys)
    | x < y = x : xs  <+> ys'
    | x > y = y : xs' <+> ys
    | otherwise = x : xs <+> ys

d235 :: [Integer]
d235 = 1 : map (* 2) d235 <+> map (* 3) d235 <+> map (* 5) d235

--------------------------------------------------------------------------------

-- Zadanie 9

instance Functor BTree where
    fmap _ BLeaf = BLeaf
    fmap f (BNode l v r) = BNode (fmap f l) (f v) (fmap f r)

natTree :: BTree Int
natTree = BNode ((* 2) <$> natTree) 1 ((+ 1) . (* 2) <$> natTree)

-- pomocnicza funkcja do testowania
-- ∀ n, takeDepth natTree n == [1 .. 2^n - 1]
takeDepth :: Int -> BTree a -> [a]
takeDepth 0 _ = []
takeDepth _ BLeaf = error "empty tree"
takeDepth n (BNode l v r) = 
    v : takeDepth (n - 1) l `interleave` takeDepth (n - 1) r
  where
    interleave [] ys = ys
    interleave xs [] = xs
    interleave (x:xs) (y:ys) = x : y : interleave xs ys

--------------------------------------------------------------------------------

-- Zadanie 10

data RoseTree a = RNode a [RoseTree a] deriving Show

onesBTree :: BTree Int
onesBTree = BNode onesBTree 1 onesBTree

onesRoseTree :: RoseTree Int
onesRoseTree = RNode 1 xs
  where
    xs = onesRoseTree : xs

--------------------------------------------------------------------------------

-- Zadanie 12

data Cyclist a = Elem (Cyclist a) a (Cyclist a)

fromList :: [a] -> Cyclist a
fromList [] = error "fromList: empty list"
fromList xs = first
  where
    (first, last) = aux xs last
    aux [x] prev = let cycle = Elem prev x first in (cycle, cycle)
    aux (x:xs) prev = (this, last)
      where
        this         = Elem prev x next
        (next, last) = aux xs this

forward :: Cyclist a -> Cyclist a
forward (Elem _ _ next) = next

backward :: Cyclist a -> Cyclist a
backward (Elem prev _ _) = prev

label :: Cyclist a -> a
label (Elem _ a _) = a

--------------------------------------------------------------------------------

-- Zadanie 13

enumInts :: Cyclist Integer
enumInts = fromList [0..]

--------------------------------------------------------------------------------
