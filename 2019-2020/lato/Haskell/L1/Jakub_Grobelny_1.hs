-- Jakub Grobelny
-- Kurs języka Haskell
-- Lista 10.03.2020

import Prelude hiding (concat, and, all, maximum)
import Data.Char (digitToInt, isDigit)
import Data.List (unfoldr)
import Data.Function(on)

-- Zadanie 1
intercalate :: [a] -> [[a]] -> [a]
intercalate _ []  = []
intercalate sep (xs:xss) = xs ++ (xss >>= (sep ++))

transpose :: [[a]] -> [[a]]
transpose []  = []
transpose xss = [ xs | (xs:_) <- xss]
    : transpose [ ys | (_:ys) <- xss, notNull ys ]
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
    | otherwise = Matrix $ zipWith (zipWith (+)) a b

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

-- Zadanie 5
newtype Natural = Natural { fromNatural :: [Word] } deriving Show

base :: Word
base = 1 + (floor . sqrt . (fromIntegral :: Word -> Double)) maxBound

-- Zadanie 6
canonicalize :: Natural -> Natural
canonicalize (Natural xs)
    | all (== 0) xs = Natural [0]
    | otherwise     = Natural $ trimZeros xs
  where
    trimZeros :: [Word] -> [Word]
    trimZeros [] = []
    trimZeros xs'@(0:xs) = case trimZeros xs of
        [] -> []
        xs  -> 0 : xs
    trimZeros (x:xs) = x : trimZeros xs

isZero :: Natural -> Bool
isZero (Natural [0]) = True
isZero _ = False

instance Num Natural where
    abs = id
    signum (Natural [0]) = 0
    signum _             = 1

    fromInteger 0 = Natural [0]
    fromInteger i = Natural $ unfoldr convert i
      where
        convert :: Integer -> Maybe (Word, Integer)
        convert 0 = Nothing
        convert i =
            Just (fromIntegral $ i `mod` toInteger base, i `div` toInteger base)

    Natural xs + Natural ys = Natural $ add 0 xs ys
      where
        add :: Word -> [Word] -> [Word] -> [Word]
        add 0     [] [] = []
        add carry [] [] = [carry]
        add carry xs [] = add 0 xs [carry]
        add carry [] ys = add 0 [carry] ys
        add carry (x:xs) (y:ys) = sum : add carry' xs ys
          where
            addBounded :: Word -> Word -> Word -> (Word, Word)
            addBounded carry x y
                | x + y + carry >= base = (x + y + carry - base, 1)
                | otherwise             = (x + y + carry,        0)
            (sum, carry') = addBounded carry x y

    Natural xs - Natural ys = canonicalize . Natural $ sub 0 xs ys
      where 
        sub :: Word -> [Word] -> [Word] -> [Word]
        sub 0      [] [] = []
        sub borrow [] [] = error "Natural underflow!"
        sub borrow xs [] = sub 0 xs [borrow]
        sub borrow [] xs = sub 0 [borrow] xs
        sub borrow (x:xs) (y:ys) = diff : sub borrow' xs ys
          where
            subBounded :: Word -> Word -> Word -> (Word, Word)
            subBounded borrow x y
                | x < y + borrow = (base + x - borrow - y, 1)
                | otherwise      = (x - y - borrow,        0)
            (diff, borrow') = subBounded borrow x y

    Natural xs * Natural ys = canonicalize $ mult xs ys
      where
        mult :: [Word] -> [Word] -> Natural
        mult xs ys = addResults $ map (multiplyByDigit ys 0) xs
        addResults :: [[Word]] -> Natural
        addResults [xs] = Natural xs
        addResults (xs:xss) = Natural xs + addResults [0 : xs | xs <- xss]
        multiplyByDigit :: [Word] -> Word -> Word -> [Word]
        multiplyByDigit [] 0 digit         = []
        multiplyByDigit [] carry digit     = [carry]
        multiplyByDigit (x:xs) carry digit = 
            snd result : multiplyByDigit xs (fst result) digit 
          where 
            result = (digit * x + carry) `divMod` base

-- Zadanie 7
instance Eq Natural where
    (==) = (==) `on` fromNatural

instance Ord Natural where
    (Natural xs) <= (Natural ys) = aux (reverse xs) (reverse ys)
      where
        aux :: [Word] -> [Word] -> Bool
        aux [] _ = True
        aux _ [] = False
        aux (x:xs) (y:ys) = x <= y && aux xs ys

-- Zadanie 10
-- Definicje są wykomentowane, ponieważ vscode podpowiadał sygnatury

-- val1 = (.)(.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- po zaaplikowaniu (.) do (.):
-- (b -> c) = (b1 -> c1) -> (a1 -> b1) -> a1 -> c1
-- b = (b1 -> c1)
-- c = (a1 -> b1) -> a1 -> c1
-- (a -> (b1 -> c1)) -> a -> (a1 -> b1) -> a1 -> c1
-- val1 :: (a -> b1 -> c1) -> a -> (a1 -> b1) -> a1 -> c1

-- val2 = (.)($)
-- ($) :: (a1 -> b1) -> a1 -> b1
-- po zaaplikowaniu (.) do ($)
-- b = (a1 -> b1) 
-- c = (a1 -> b1)
-- (a -> (a1 -> b1)) -> a -> (a1 -> b1)
-- val2 :: (a -> a1 -> b1) -> a -> a1 -> b1

-- val3 = ($)(.)
-- val3 :: (b -> c) -> (a -> b) -> a -> c

-- val4 = flip flip
-- flip :: (a -> b -> c) -> b -> a -> c
-- val4 :: b -> (a -> b -> c) -> a -> c

-- val5 = (.)(.)(.)
-- (.)(.) :: (a -> b1 -> c1) -> a -> (a1 -> b1) -> a1 -> c1
-- (.) :: (b2 -> c2) -> (a2 -> b2) -> a2 -> c2
-- a = (b2 -> c2)
-- b1 = (a2 -> b2)
-- c1 = (a2 -> c2)
-- val5 :: (b2 -> c2) -> (a1 -> a2 -> b2) -> a1 -> a2 -> c2

-- val6 = (.)($)(.)
-- (.)($) :: (a -> a1 -> b1) -> a -> a1 -> b1
-- (.) = (b2 -> c2) -> (a2 -> b2) -> a2 -> c2
-- a  = (b2 -> c2)
-- a1 = (a2 -> b2)
-- b1 = (a2 -> c2)
-- (b2 -> c2) -> (a2 -> b2) -> (a2 -> c2)
-- val6 :: (b2 -> c2) -> (a2 -> b2) -> a2 -> c2

-- val7 = ($)(.)(.)
-- ($)(.) :: (b -> c) -> (a -> b) -> a -> c
-- ^^^ to samo co typ (.) a (.)(.) już policzyliśmy
-- val7 ::  (a -> b1 -> c1) -> a -> (a1 -> b1) -> a1 -> c1

-- val8 = flip flip flip
-- flip :: (a -> b -> c) -> b -> a -> c
-- flip flip :: b1 -> (a1 -> b1 -> c1) -> a1 -> c1
-- b1 = (a -> b -> c) -> b -> a -> c
-- val8 :: (a1 -> ((a -> b -> c) -> b -> a -> c) -> c1) -> a1 -> c1

-- val9 = tail $ map tail [[], ['a']]
-- map :: (a -> b) -> [a] -> [b]
-- tail :: [a] -> [a]
-- [[], ['a']] :: [[Char]]
-- map tail [[], ['a']] :: [[Char]]
-- tail $ map tail [[], ['a']] :: [[Char]]
-- val9 :: [[Char]]

-- val10 = let x = x in x x
-- x ma polimorficzny typ (? first-class polymorphism ?)
-- val10 :: a

-- val11 = (\_ -> 'a') (head [])
-- head :: [a] -> a
-- head [] :: a
-- \_ -> 'a' :: a1 -> Char
-- val11 :: Char

-- val12 = (\(_,_) -> 'a') (head [])
-- head [] :: a
-- \(_,_) -> 'a' :: (a1, b) -> Char
-- a = (a1, b)
-- val12 :: Char

-- val13 = map map
-- map :: (a -> b) -> [a] -> [b]
-- a = (a1 -> b1)
-- b = [a1] -> [b1]
-- val13 :: [a1 -> b1] -> [[a1] -> [b1]]

-- val14 = map flip
-- flip :: (a -> b -> c) -> b -> a -> c
-- map :: (a1 -> b1) -> [a1] -> [b1]
-- a1 = a -> b -> c
-- b1 = b -> a -> c
-- val14 :: [a -> b -> c] -> [b -> a -> c]

-- val15 = flip map
-- val15 :: [a] -> (a -> b) -> [b]