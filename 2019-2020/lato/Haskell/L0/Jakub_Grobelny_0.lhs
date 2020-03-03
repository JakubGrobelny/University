Jakub Grobelny
Kurs języka Haskell
Lista 0, 02.03.2020

Zadanie 3

> import Prelude hiding (elem)
> import Data.List ( unfoldr
>                  , reverse
>                  , concatMap
>                  , delete
>                  , minimum
>                  , partition
>                  , insert
>                  )
> import Data.Char (isAlpha)

> explode :: Integer -> [Integer]
> explode 0 = [0]
> explode n = reverse $ unfoldr aux n
>   where
>     aux :: Integer -> Maybe (Integer, Integer)
>     aux 0 = Nothing
>     aux n = Just (n `mod` 10, n `div` 10)

> implode :: [Integer] -> Integer
> implode = foldl (\acc digit -> acc * 10 + digit) 0

> rot13 :: String -> String
> rot13 = map $ shift 13
>   where
>     shift :: Int -> Char -> Char
>     shift n c = iterate next c !! n
>     next :: Char -> Char
>     next c
>         | c == 'z'  = 'a'
>         | c == 'Z'  = 'A'
>         | isAlpha c = succ c
>         | otherwise = c

> subsequences :: [a] -> [[a]]
> subsequences [] = [[]]
> subsequences (x:xs) = concatMap (\ys -> [ys, x : ys]) yss
>   where
>     yss = subsequences xs

> inits :: [a] -> [[a]]
> inits [] = [[]]
> inits (x:xs) = [] : map (x :) (inits xs)

> tails :: [a] -> [[a]]
> tails [] = [[]]
> tails xs'@(x:xs) = xs' : tails xs

> segments :: [a] -> [[a]]
> segments [] = [[]]
> segments (x:xs) = map (x :) ys ++ segments xs
>   where
>     ys = inits xs

> permutations :: [a] -> [[a]]
> permutations [] = [[]]
> permutations (x:xs) = concatMap (insert x) $ permutations xs
>   where
>     insert :: a -> [a] -> [[a]]
>     insert x []     = [[x]]
>     insert x (y:ys) = (x:y:ys) : map (y :) (insert x ys)

> merge :: Ord a => [a] -> [a] -> [a]
> merge [] xs = xs
> merge xs [] = xs
> merge xs'@(x:xs) ys'@(y:ys)
>     | x <= y    = x : merge xs ys'
>     | otherwise = y : merge xs' ys

> msortPrefix :: Ord a => Int -> [a] -> [a]
> msortPrefix 0 _ = []
> msortPrefix 1 (x:_) = [x]
> msortPrefix n xs = ys `merge` zs
>   where
>     half = n `div` 2
>     ys = msortPrefix half xs
>     zs = msortPrefix (n - half) (drop half xs)

> msort :: Ord a => [a] -> [a]
> msort xs = msortPrefix (length xs) xs

> qsort :: Ord a => [a] -> [a]
> qsort [] = []
> qsort (x:xs) = qsort lesser ++ x : qsort greater
>   where
>     (lesser, greater) = partition (< x) xs

> isort :: Ord a => [a] -> [a]
> isort = foldr insert []

> ssort :: Ord a => [a] -> [a]
> ssort [] = []
> ssort xs = min : ssort (delete min xs)
>   where
>     min = minimum xs

> elem :: Eq a => a -> [a] -> Bool
> elem = any . (==)

> intersperse :: a -> [a] -> [a]
> intersperse _ [] = []
> intersperse sep (x:xs) = x : concatMap (\x -> [sep, x]) xs
