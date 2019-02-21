{-# LANGUAGE RankNTypes #-}

newtype Church = Church (forall a. (a -> a) -> (a -> a))

unChurch :: Church -> (forall a. (a -> a) -> (a -> a))
unChurch (Church n) = n

zero :: Church
zero = Church (\f x -> x)

isZero :: Church -> Bool
isZero (Church n) = n (\x -> False) True

successor :: Church -> Church
successor (Church n) = Church (\f x -> f (n f x))

predecessor :: Church -> Church
predecessor (Church n) = 
    Church (\f x -> n (\g h -> h (g f)) (\u -> x) (\u -> u))

add :: Church -> Church -> Church
add (Church m) (Church n) = Church (\f x -> m f (n f x))

multiply :: Church -> Church -> Church
multiply (Church m) (Church n) = Church (\f -> m (n f))

substract :: Church -> Church -> Church
substract m n = unChurch n predecessor m

intToChurch :: Integer -> Church
intToChurch 0 = zero
intToChurch n = successor $ intToChurch $ n -1

churchToInt :: Church -> Integer
churchToInt n
    | isZero n = 0
    | otherwise = 1 + (churchToInt $ predecessor n)

instance Eq Church where
    n == m
        | isZero n = isZero m
        | otherwise = predecessor n == predecessor m

instance Show Church where
    show n = show $ churchToInt n

instance Ord Church where
    compare n m = compare (churchToInt n) (churchToInt m)
    n <= m = (churchToInt n) <= (churchToInt m)

instance Num Church where
    n + m
        | isZero m = n
        | otherwise = successor n + predecessor m
    n - m
        | n <= m = zero
        | isZero m = n
        | otherwise = predecessor n - predecessor m
    n * m = multiply n m
    abs n = n
    signum n
        | isZero n = zero
        | otherwise = successor zero
    fromInteger = intToChurch
    negate n = n