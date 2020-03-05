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
    | length u' /= length v' = error "Vector length mismatch!"
    | otherwise = sum $ zipWith (*) u' v'
  where
    u' = fromVector u
    v' = fromVector v

sumV :: Num a => Vector a -> Vector a -> Vector a
sumV u v
    | length u' /= length v' = error "Vector length mismatch!"
    | otherwise = Vector $ zipWith (+) u' v'
  where
    u' = fromVector u
    v' = fromVector v

-- Zadanie 3
newtype Matrix a = Matrix { fromMatrix :: [[a]] } deriving Show

-- Pomocnicza funkcja do wyznaczania rozmiaru macierzy
dimM :: Matrix a -> (Int, Int)
dimM m = (rows, expectColumnLength colLengths)
  where
    m' = fromMatrix m
    rows = length m'
    colLengths = map length m'
    expectColumnLength :: [Int] -> Int
    expectColumnLength [] = error "Misshapen matrix!"
    expectColumnLength (x:xs)
        | all (== x) xs = x
        | otherwise = error "Misshapen matrix!"

sumM :: Num a => Matrix a -> Matrix a -> Matrix a
sumM a b
    | dimM a /= dimM b = error "Matrix size mismatch!"
    | otherwise = Matrix $ map (uncurry $ zipWith (+)) $ zip a' b'
  where
    a' = fromMatrix a
    b' = fromMatrix b

prodM :: Num a => Matrix a -> Matrix a -> Matrix a
prodM a b
    | matching a b = undefined --TODO: finish
    | otherwise    = error "Matrix size mismatch!"
  where
    matching :: Matrix a -> Matrix a -> Bool
    matching a b = snd (dimM a) == fst (dimM b)
    a' = fromMatrix a
    b' = fromMatrix b