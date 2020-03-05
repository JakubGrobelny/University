-- Jakub Grobelny
-- Kurs języka Haskell
-- Lista 10.03.2020

-- Zadanie 1
import Prelude hiding (concat, and, all, maximum)

intercalate :: [a] -> [[a]] -> [a]
intercalate _ []  = []
intercalate sep (xs:xss) = xs ++ (xss >>= (sep ++))

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose xss 
    | all null tails = [heads]
    | otherwise      = heads : transpose tails
  where
    separateHeads :: [[a]] -> [(a, [a])]
    separateHeads = map (\xs -> (head xs, tail xs)) . filter (not . null)
    separated = separateHeads xss
    (heads, tails) = (map fst separated, map snd separated)

concat :: [[a]] -> [a]
concat = foldr (++) []

and :: [Bool] -> Bool
and = foldr (&&) True

all :: (a -> Bool) -> [a] -> Bool
all pred = foldr ((&&) . pred) True

maximum :: [Integer] -> Integer
maximum [] = undefined
maximum (x:xs) = foldr max x xs