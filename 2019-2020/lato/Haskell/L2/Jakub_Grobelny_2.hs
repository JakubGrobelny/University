-- Jakub Grobelny
-- Kurs języka Haskell
-- Lista 2, 20.03.2020

-- Zadanie 1
{-# LANGUAGE ParallelListComp #-}

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
qsortC (x:xs) = [y | y <- xs, y < x] ++ [x] ++ [y | y <- xs, y >= x]

zipC :: [a] -> [b] -> [(a,b)]
zipC xs ys = [(x, y) | x <- xs | y <- ys]

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
    show (lhs :$ rhs) = show lhs ++ show rhs
