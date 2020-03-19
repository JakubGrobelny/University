-- Jakub Grobelny
-- Kurs języka Haskell
-- Lista 2, 20.03.2020

{-# LANGUAGE ParallelListComp #-}

import Data.Bool (bool)
import Data.Function (on)
import Control.Monad (join)
import Control.Arrow ((&&&), (***))

-- Zadanie 1

subseqC :: [a] -> [[a]]
subseqC [] = [[]]
subseqC (x:xs) = concat [[x:ys, ys] | ys <- subseqC xs]

ipermC :: [a] -> [[a]]
ipermC [] = [[]]
ipermC (x:xs) = concat [insert x ys | ys <- ipermC xs]
  where
    insert :: a -> [a] -> [[a]]
    insert x [] = [[x]]
    insert x ys'@(y:ys) = (x:ys') : [ y : zs | zs <- insert x ys]

spermC :: [a] -> [[a]]
spermC [] = [[]]
spermC xs = [y:zs | (y, ys) <- select xs, zs <- spermC ys]
  where
    select :: [a] -> [(a, [a])]
    select [x] = [(x, [])]
    select (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- select xs]

qsortC :: Ord a => [a] -> [a]
qsortC [] = []
qsortC (x:xs) = qsortC [y | y <- xs, y < x] 
             ++ [x] 
             ++ qsortC [y | y <- xs, y >= x]

zipC :: [a] -> [b] -> [(a,b)]
zipC xs ys = [(x, y) | x <- xs | y <- ys]

-- Zadanie 2

subseqP :: [a] -> [[a]]
subseqP = join $ bool subseq pure . null
  where
    subseq = uncurry append . (head &&& subseqP . tail)
    append = join . ((++) .) . map . (:)

ipermP :: [a] -> [[a]]
ipermP = undefined

spermP :: [a] -> [[a]]
spermP = undefined

-- uwaga: działa tylko dla list o równej długości (tak jak zipF z listy zadań)
zipP :: [a] -> [b] -> [(a,b)]
zipP = (join . join) $ bool zipOnce (const $ const []) .: bothNull
  where
    bothNull = uncurry (&&) .: curry (null *** null)
    heads = (head *** head)
    tails = (tail *** tail)
    zipOnce = uncurry (:) .: ((heads &&& (uncurry zipP . tails)) .: (,))
    (.:) = (.) . (.)

qsortP :: Ord a => [a] -> [a]
qsortP = join $ bool sort id . null
  where
    sort  = join . (split <*>) . pure
    split = [ qsortP . extract (>)
            ,          extract (==)
            , qsortP . extract (<)
            ]
    extract = (>>= filter) . (. head)

(<++>) :: Ord a => [a] -> [a] -> [a]
(<++>) = undefined

-- Zadanie 3
data Combinator 
    = S 
    | K 
    | Combinator :$ Combinator

infixl :$

instance Show Combinator where
    show S = "S"
    show K = "K"
    show (lhs :$ rhs@(_:$_)) = show lhs ++ "(" ++ show rhs ++ ")"
    show (lhs :$ rhs)        = show lhs ++ show rhs

-- Zadanie 5
data BST a
    = NodeBST (BST a) a (BST a)
    | EmptyBST
    deriving Show

searchBST :: Ord a => a -> BST a -> Maybe a
searchBST _ EmptyBST = Nothing
searchBST a (NodeBST left val right)
    | a == val  = Just a
    | a <  val  = searchBST a left
    | otherwise = searchBST a right

insertBST :: Ord a => a -> BST a -> BST a
insertBST a EmptyBST = NodeBST EmptyBST a EmptyBST
insertBST a tree@(NodeBST left val right)
    | a == val  = tree
    | a <  val  = NodeBST (insertBST a left) val right
    | otherwise = NodeBST left val (insertBST a right)

-- Zadanie 6

deleteMaxBST :: Ord a => BST a -> (BST a, a)
deleteMaxBST EmptyBST = error "deleteMaxBST: empty tree!"
deleteMaxBST (NodeBST left val EmptyBST) = (left, val)
deleteMaxBST (NodeBST left val right) = (NodeBST left val right', max)
  where
    (right', max) = deleteMaxBST right

deleteBST :: Ord a => a -> BST a -> BST a
deleteBST _ EmptyBST = EmptyBST
deleteBST a (NodeBST left val right)
    | a < val   = NodeBST (deleteBST a left) val right
    | a > val   = NodeBST left val (deleteBST a right)
    | otherwise = case left of
        EmptyBST -> right
        _        -> uncurry NodeBST (deleteMaxBST left) right

-- Zadanie 7
data Tree23 a 
    = Node2 (Tree23 a) a (Tree23 a)
    | Node3 (Tree23 a) a (Tree23 a) a (Tree23 a)
    | Empty23
    deriving Show

search23 :: Ord a => a -> Tree23 a -> Maybe a
search23 _ Empty23 = Nothing
search23 a (Node2 left val right)
    | a == val  = Just a
    | a <  val  = search23 a left
    | otherwise = search23 a right
search23 a (Node3 left val1 mid val2 right)
    | a == val1 || a == val2 = Just a
    | a < val1  = search23 a left
    | a < val2  = search23 a mid
    | otherwise = search23 a right

-- Zadanie 8

data InsertResult a 
    = Balanced (Tree23 a)
    | Grown    (Tree23 a) a (Tree23 a)

insert23 :: Ord a => a -> Tree23 a -> Tree23 a
insert23 a tree = case insert' a tree of
    Balanced tree' -> tree'
    Grown l v r    -> Node2 l v r

insert' :: Ord a => a -> Tree23 a -> InsertResult a
insert' x Empty23 = Grown Empty23 x Empty23
insert' x tree@(Node2 left val right)
    | x < val   = case insert' x left of
        Balanced left'          -> Balanced $ Node2 left' val right
        Grown lleft lval lright -> Balanced $ Node3 lleft lval lright val right
    | x > val   = case insert' x right of
        Balanced right'         -> Balanced $ Node2 left val right'
        Grown rleft rval rright -> Balanced $ Node3 left val rleft rval rright
    | otherwise = Balanced tree
insert' x tree@(Node3 left a1 mid a2 right)
    | x <  a1 = case insert' x left of
        Balanced left'          -> Balanced $ Node3 left' a1 mid a2 right
        Grown lleft lval lright -> 
            Grown (Node2 lleft lval lright) a1 (Node2 mid a2 right)
    | x == a1 = Balanced tree
    | x <  a2 = case insert' x mid of
        Balanced mid'           -> Balanced $ Node3 left a1 mid' a2 right
        Grown mleft mval mright ->
            Grown (Node2 left a1 mleft) mval (Node2 mright a2 right)
    | x == a2 = Balanced tree
    | otherwise = case insert' x right of
        Balanced right'         -> Balanced $ Node3 left a1 mid a2 right'
        Grown rleft rval rright ->
            Grown (Node2 left a1 mid) a2 (Node2 rleft rval rright)

-- Pomocnicze funkcje do testowania 2-3-drzew
listTo23Tree :: Ord a => [a] -> Tree23 a
listTo23Tree [] = Empty23
listTo23Tree (x:xs) = insert23 x $! listTo23Tree xs

pathLengths :: Tree23 a -> Maybe Int
pathLengths Empty23 = Just 0
pathLengths (Node2 l _ r) = do
    llen <- pathLengths l
    rlen <- pathLengths r
    if llen == rlen
        then return $! llen + 1
        else Nothing
pathLengths (Node3 l _ m _ r) = do
    llen <- pathLengths l
    mlen <- pathLengths m
    rlen <- pathLengths r
    if llen == mlen && mlen == rlen
        then return $! llen + 1
        else Nothing