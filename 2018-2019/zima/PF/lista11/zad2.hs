{-# LANGUAGE KindSignatures, MultiParamTypeClasses, FlexibleInstances #-}

import Data.List (unfoldr)
import Data.Bool (bool)

(><) :: (a -> b) -> (a -> c) -> a -> (b, c)
(f >< g) x = (f x, g x)

warbler :: (a -> a -> b) -> a -> b
warbler f x = f x x

class Ord a => Prioq (t :: * -> *) (a :: *) where
    empty       :: t a
    isEmpty     :: t a -> Bool
    single      :: a -> t a
    insert      :: a -> t a -> t a
    merge       :: t a -> t a -> t a
    extractMin  :: t a -> (a, t a)
    findMin     :: t a -> a
    deleteMin   :: t a -> t a
    fromList    :: [a] -> t a
    toList      :: t a -> [a]
    
    insert = merge . single
    single = flip insert empty
    extractMin = findMin >< deleteMin
    findMin = fst . extractMin
    deleteMin = snd . extractMin
    fromList = foldr insert empty
    toList = 
        unfoldr . warbler $ bool (Just . extractMin) (const Nothing) . isEmpty

newtype ListPrioq a = LP { unLP :: [a] } deriving Show

instance Ord a => Prioq ListPrioq a where
    empty = LP { unLP = [] }
    isEmpty xs = (unLP xs) == []
    merge q1 q2 = LP $ merge' (unLP q1) (unLP q2)
        where
            merge' :: Ord a => [a] -> [a] -> [a]
            merge' [] q = q
            merge' q [] = q
            merge' q1@(x:xs) q2@(y:ys)
                | x < y = x : merge' xs q2
                | otherwise = y : merge' q1 ys
    single a = LP [a]
    insert a q = LP $ insert' a (unLP q)
        where
            insert' :: Ord a => a -> [a] -> [a]
            insert' a [] = [a]
            insert' a q@(x:xs)
                | a < x = a : q
                | otherwise = x : insert' a xs
    extractMin (LP []) = error "extractMin: empty queue!"
    extractMin (LP (q:qs)) = (q, LP qs)

-- Przykład: findMin $ insert 5 (empty :: ListPrioq Int)