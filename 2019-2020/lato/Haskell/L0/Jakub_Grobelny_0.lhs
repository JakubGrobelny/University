Jakub Grobelny
Kurs języka Haskell
Lista 0, 02.03.2020

Zadanie 3

> {-# LANGUAGE LambdaCase #-}
> import Prelude hiding (elem)
> import Data.List (unfoldr, reverse)

> implode :: [Integer] -> Integer
> implode = foldl (\acc digit -> acc * 10 + digit) 0

> elem :: Eq a => a -> [a] -> Bool
> elem = any . (==)

> inits :: [a] -> [[a]]
> inits [] = [[]]
> inits (x:xs) = [] : (map (x :) $ inits xs)

> tails :: [a] -> [[a]]
> tails [] = [[]]
> tails (x:xs) = (x : head tails') : tails'
>   where
>     tails' = tails xs

> subsequences :: [a] -> [[a]]
> subsequences [] = [[]]
> subsequences (x:xs) = yss >>= \ys -> [ys, x : ys]
>   where
>     yss = subsequences xs

