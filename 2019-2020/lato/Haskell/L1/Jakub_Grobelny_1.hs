-- Jakub Grobelny
-- Kurs języka Haskell
-- Lista 10.03.2020

import Prelude hiding (concat, and, all, maximum)
import Data.Char (digitToInt, isDigit)

-- Zadanie 1
intercalate :: [a] -> [[a]] -> [a]
intercalate _ []  = []
intercalate sep (xs:xss) = xs ++ (xss >>= (sep ++))

transpose :: [[a]] -> [[a]]
transpose []  = []
transpose xss = [ head xs | xs     <- xss, notNull xs ]
    : transpose [ ys      | (_:ys) <- xss, notNull ys ]
  where notNull = not . null

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
norm = sqrt . sum . map (^ 2) . fromVector

scalarProd :: Num a => Vector a -> Vector a -> a
scalarProd (Vector u) (Vector v)
    | length u /= length v = error "Vector length mismatch!"
    | otherwise = sum $ zipWith (*) u v

sumV :: Num a => Vector a -> Vector a -> Vector a
sumV (Vector u) (Vector v)
    | length u /= length v = error "Vector length mismatch!"
    | otherwise            = Vector $ zipWith (+) u v

-- Zadanie 3
newtype Matrix a = Matrix { fromMatrix :: [[a]] } deriving Show

-- Pomocnicza funkcja do wyznaczania rozmiaru macierzy
dimM :: Matrix a -> (Int, Int)
dimM (Matrix m) = (rows, expectColumnLength colLengths)
  where
    rows = length m
    colLengths = map length m
    expectColumnLength :: [Int] -> Int
    expectColumnLength [] = error "Misshapen matrix!"
    expectColumnLength (x:xs)
        | all (== x) xs = x
        | otherwise     = error "Misshapen matrix!"

sumM :: Num a => Matrix a -> Matrix a -> Matrix a
sumM a'@(Matrix a) b'@(Matrix b)
    | dimM a' /= dimM b' = error "Matrix size mismatch!"
    | otherwise = Matrix $ map (uncurry $ zipWith (+)) $ zip a b

prodM :: Num a => Matrix a -> Matrix a -> Matrix a
prodM a'@(Matrix a) b'@(Matrix b)
    | matchingDimensions a' b' = Matrix $ prod a b
    | otherwise                = error "Matrix size mismatch!"
  where
    matchingDimensions :: Matrix a -> Matrix a -> Bool
    matchingDimensions a b = snd (dimM a) == fst (dimM b)
    prod :: Num a => [[a]] -> [[a]] -> [[a]]
    prod xss yss = do
        row <- Vector <$> xss
        return $ scalarProd row . Vector <$> transpose yss

det :: Num a => Matrix a -> a
det m@(Matrix xss)
    | isSquare m  = det' xss
    | otherwise   = error "Nonsquare matrix!"
  where
    det' :: Num a => [[a]] -> a
    det' [[a]] = a
    det' xss = sum [signedElem xss col * minorDet col | col <- [1..n] ]
      where
        n        = length xss
        minorDet = det' . minor xss
    isSquare :: Matrix a -> Bool
    isSquare = uncurry (==) . dimM
    minor :: [[a]] -> Int -> [[a]]
    minor xss colIndex = do
        row <- tail xss
        return $ row `without` (colIndex - 1)
      where
        without :: [a] -> Int -> [a]
        without [] _ = error "'without' called on empty list!"
        without (_:xs) 0 = xs
        without (x:xs) n = x : xs `without` (n-1)
    signedElem :: Num a => [[a]] -> Int -> a
    signedElem xss col = (-1) ^ (1 + col) * (head xss !! (col - 1))

-- Zadanie 4

isbn13Check :: String -> Bool
isbn13Check = (== 0) . (`mod` 10) . sum . zipWith (*) weights . toDigits
  where
    toDigits :: String -> [Int]
    toDigits = map digitToInt . filter isDigit
    weights = cycle [1, 3]