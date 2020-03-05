-- Jakub Grobelny
-- Kurs języka Haskell
-- Lista 10.03.2020

import Prelude hiding (concat, and, all, maximum)

-- Zadanie 1
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

-- Zadanie 2
newtype Vector a = Vector { fromVector :: [a] } deriving Show

scaleV :: Num a => a -> Vector a -> Vector a
scaleV scalar = Vector . map (scalar *) . fromVector

norm :: Floating a => Vector a -> a
norm = sqrt . sum . fromVector

scalarProd :: Num a => Vector a -> Vector a -> a
scalarProd u v
    | length ux /= length vx = error "Vector length mismatch!"
    | otherwise = sum $ zipWith (*) ux vx
  where
    ux = fromVector u
    vx = fromVector v

sumV :: Num a => Vector a -> Vector a -> Vector a
sumV u v
    | length ux /= length vx = error "Vector length mismatch!"
    | otherwise = Vector $ zipWith (+) ux vx
  where
    ux = fromVector u
    vx = fromVector v