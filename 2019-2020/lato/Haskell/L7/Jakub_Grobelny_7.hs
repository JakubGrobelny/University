-- Jakub Grobelny
-- Kurs języka Haskell
-- Lista 7, 24.04.2020

{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

import Prelude hiding (pred, succ)

--------------------------------------------------------------------------------

-- Zadanie 3

data Expr a where
    C     :: a -> Expr a
    P     :: (Expr a, Expr b) -> Expr (a, b)
    Not   :: Expr Bool -> Expr Bool
    (:+)  :: Expr Integer -> Expr Integer -> Expr Integer 
    (:-)  :: Expr Integer -> Expr Integer -> Expr Integer 
    (:*)  :: Expr Integer -> Expr Integer -> Expr Integer
    (:/)  :: Expr Integer -> Expr Integer -> Expr (Integer, Integer)
    (:<)  :: Expr Integer -> Expr Integer -> Expr Bool 
    (:>)  :: Expr Integer -> Expr Integer -> Expr Bool 
    (:<=) :: Expr Integer -> Expr Integer -> Expr Bool 
    (:>=) :: Expr Integer -> Expr Integer -> Expr Bool 
    (:!=) :: Expr Integer -> Expr Integer -> Expr Bool 
    (:==) :: Expr Integer -> Expr Integer -> Expr Bool
    (:&&) :: Expr Bool -> Expr Bool -> Expr Bool 
    (:||) :: Expr Bool -> Expr Bool -> Expr Bool
    (:?)  :: Expr Bool -> Expr a -> Expr a -> Expr a
    Fst   :: Expr (a, b) -> Expr a
    Snd   :: Expr (a, b) -> Expr b

infixl 6 :*, :/
infixl 5 :+, :-
infixl 4 :<, :>, :<=, :>=, :!=, :==
infixl 3 :&&
infixl 2 :||
infixl 1 :?

eval :: Expr a -> a
eval (C a) = a
eval (P (e1, e2)) = (eval e1, eval e2)
eval (Not e) = not . eval $ e
eval (e1 :+ e2) = eval e1 + eval e2
eval (e1 :- e2) = eval e1 - eval e2
eval (e1 :* e2) = eval e1 * eval e2
eval (e1 :/ e2) = eval e1 `divMod` eval e2
eval (e1 :< e2) = eval e1 < eval e2
eval (e1 :> e2) = eval e1 > eval e2
eval (e1 :<= e2) = eval e1 <= eval e2
eval (e1 :>= e2) = eval e1 >= eval e2
eval (e1 :!= e2) = eval e1 /= eval e2
eval (e1 :== e2) = eval e1 == eval e2
eval (e1 :&& e2) = eval e1 && eval e2
eval (e1 :|| e2) = eval e1 || eval e2
eval ((:?) cond then' else') = if eval cond then eval then' else eval else'
eval (Fst e) = fst . eval $ e
eval (Snd e) = snd . eval $ e

--------------------------------------------------------------------------------

-- Zadanie 6

newtype Church = Church (forall a . (a -> a) -> (a -> a))

zero :: Church
zero = Church $ const id

succ :: Church -> Church
succ (Church n) = Church $ \f -> f . n f

pred :: Church -> Church
pred (Church n) = Church $ \f x -> n (\g h -> h (g f)) (const x) id

isZero :: Church -> Bool
isZero (Church n) = n (const False) True

instance Eq Church where
    n == m
        | isZero n  = isZero m
        | isZero m  = isZero n
        | otherwise = pred n == pred m

instance Ord Church where
    n <= m
        | isZero m  = isZero n
        | otherwise = pred n <= pred m

instance Num Church where
    (Church n) + (Church m) = Church $ \f -> n f . m f

    (Church n) * (Church m) = Church $ n . m

    n - m
        | isZero n  = zero
        | isZero m  = n
        | otherwise = pred n - pred m

    abs = id

    signum n
        | isZero n  = 0
        | otherwise = 1

    fromInteger 0 = zero
    fromInteger n = succ . fromInteger $ n - 1

instance Show Church where
    show = show . toInteger
      where
        toInteger :: Church -> Integer
        toInteger n
            | isZero n  = 0
            | otherwise = 1 + toInteger (pred n)

--------------------------------------------------------------------------------

-- Zadanie 7

newtype CList x = CList (forall a . (x -> a -> a) -> a -> a)

empty :: CList x
empty = CList $ flip const

cons :: x -> CList x -> CList x
cons x (CList xs) = CList $ \f n -> f x (xs f n)

append :: CList x -> CList x -> CList x
append (CList xs) (CList ys) = CList $ \f n -> xs f (ys f n)

fromList :: [x] -> CList x
fromList = foldr cons empty

toList :: CList x -> [x]
toList (CList xs) = xs (:) []

--------------------------------------------------------------------------------

-- Zadanie 8

newtype MList x = MList (forall m . Monoid m => (x -> m) -> m)

empty' :: MList x
empty' = MList $ const mempty

cons' :: x -> MList x -> MList x
cons' x (MList xs) = MList $ \f -> f x <> xs f

append' :: MList x -> MList x -> MList x
append' (MList xs) (MList ys) = MList $ \f -> xs f <> ys f

fromList' :: [x] -> MList x
fromList' = foldr cons' empty'

toList' :: MList x -> [x]
toList' (MList xs) = xs pure

--------------------------------------------------------------------------------

