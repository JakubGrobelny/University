Jakub Grobelny
Kurs języka Haskell
Lista 0, 02.03.2020

Zadanie 3

> import Prelude hiding (elem)
> import Data.List (unfoldr, reverse, concatMap)
> import Data.Char (ord, chr)


> explode :: Integer -> [Integer]
> explode = reverse . unfoldr aux
>   where
>     aux :: Integer -> Maybe (Integer, Integer)
>     aux 0 = Nothing
>     aux n = Just (n `mod` 10, n `div` 10)

> implode :: [Integer] -> Integer
> implode = foldl (\acc digit -> acc * 10 + digit) 0

> subsequences :: [a] -> [[a]]
> subsequences [] = [[]]
> subsequences (x:xs) = yss >>= \ys -> [ys, x : ys]
>   where
>     yss = subsequences xs

> inits :: [a] -> [[a]]
> inits [] = [[]]
> inits (x:xs) = [] : (map (x :) $ inits xs)

> tails :: [a] -> [[a]]
> tails [] = [[]]
> tails (x:xs) = (x : head tails') : tails'
>   where
>     tails' = tails xs

> merge :: Ord a => [a] -> [a] -> [a]
> merge [] xs = xs
> merge xs [] = xs
> merge xs'@(x:xs) ys'@(y:ys)
>     | x <= y    = x : merge xs ys'
>     | otherwise = y : merge xs' ys

> elem :: Eq a => a -> [a] -> Bool
> elem = any . (==)

> intersperse :: a -> [a] -> [a]
> intersperse _ [] = []
> intersperse sep (x:xs) = x : concatMap (\x -> [sep, x]) xs
