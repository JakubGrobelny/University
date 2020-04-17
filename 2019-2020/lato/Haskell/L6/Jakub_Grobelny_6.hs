-- Jakub Grobelny
-- Kurs języka Haskell
-- Lista 6, 17.04.2020

{-# LANGUAGE LambdaCase #-}

import Data.Function (on)
import Control.Monad (mapM, void)
import Data.Functor (($>))

--------------------------------------------------------------------------------

-- Zadanie 2

class Set s where
    emptyS  :: s a
    searchS :: Ord a => a -> s a -> Maybe a
    insertS :: Ord a => a -> s a -> s a
    delMaxS :: Ord a => s a -> Maybe (a, s a)
    deleteS :: Ord a => a -> s a -> s a

class Dictionary d where
    emptyD  :: d k v
    searchD :: Ord k => k -> d k v -> Maybe v
    insertD :: Ord k => k -> v -> d k v -> d k v
    deleteD :: Ord k => k -> d k v -> d k v

data KeyValue key value = KeyValue { key :: key, value :: value }

newtype SetToDict s k v = SetToDict (s (KeyValue k v))

instance Eq k => Eq (KeyValue k v) where
    (==) = (==) `on` key

instance Ord k => Ord (KeyValue k v) where
    compare = compare `on` key

instance Set s => Dictionary (SetToDict s) where
    emptyD = SetToDict emptyS

    searchD k (SetToDict s) = 
        value <$> searchS (KeyValue {key = k, value = undefined}) s

    insertD k v (SetToDict set) = 
        SetToDict $ insertS (KeyValue {key = k, value = v}) set

    deleteD k (SetToDict set) =
        SetToDict $ deleteS (KeyValue {key = k, value = undefined}) set

--------------------------------------------------------------------------------

-- Zadanie 3

data PrimRec
    = Zero
    | Succ
    | Proj Int Int
    | Comb PrimRec [PrimRec]
    | Rec PrimRec PrimRec
  deriving Show

arityCheck :: PrimRec -> Maybe Int
arityCheck Zero = return 1
arityCheck Succ = return 1
arityCheck (Proj i n)
    | 1 <= i && i <= n = return n
    | otherwise        = Nothing
arityCheck (Comb f gs) = do
    fArity    <- arityCheck f
    gsArities <- mapM arityCheck gs
    if fArity == length gsArities && allEqual gsArities
        then return . head $ gsArities
        else Nothing
  where
    allEqual :: Eq a => [a] -> Bool
    allEqual [] = True
    allEqual (x:xs) = all (== x) xs
arityCheck (Rec g h) = do
    gArity <- arityCheck g
    hArity <- arityCheck h
    if hArity - gArity /= 2
        then Nothing
        else return $ gArity + 1

--------------------------------------------------------------------------------

-- Zadanie 4

evalPrimRec :: PrimRec -> [Integer] -> Integer
evalPrimRec f args = case evalPrimRec' f args of
    Left err  -> error err
    Right res -> res
  where
    evalPrimRec' :: PrimRec -> [Integer] -> Either String Integer
    evalPrimRec' Zero args = checkArguments 1 args $> 0
    evalPrimRec' Succ args = (+ 1) . head <$> checkArguments 1 args
    evalPrimRec' p@(Proj i n) args = do
        arity <- arityCheck' p
        (!! (i - 1)) <$> checkArguments arity args
    evalPrimRec' c@(Comb f gs) args = do
        arity <- arityCheck' c
        void $ checkArguments arity args
        results <- mapM (`evalPrimRec'` args) gs
        evalPrimRec' f results
    evalPrimRec' r@(Rec g h) args = do
        arity <- arityCheck' r
        void $ checkArguments arity args
        case args of
            []     -> undefined
            0:args -> evalPrimRec' g args
            m:args -> do 
                let n = m - 1
                res <- evalPrimRec' r $ n:args
                evalPrimRec' h $ n : res : args
    arityCheck' :: PrimRec -> Either String Int
    arityCheck' f = case arityCheck f of
        Nothing -> Left "Invalid arity"
        Just a  -> return a
    checkArguments :: Int -> [Integer] -> Either String [Integer]
    checkArguments arity args
        | hasNegatives args    = Left "Negative argument"
        | arity /= length args = Left "Arity mismatch"
        | otherwise            = return args
      where
        hasNegatives :: (Ord a, Num a) => [a] -> Bool
        hasNegatives = any (< 0)

-- Test:
add :: Integer -> Integer -> Integer
add a b = evalPrimRec f [a, b]
  where
    f = (Rec (Proj 1 1) (Comb Succ [Proj 2 3]))

--------------------------------------------------------------------------------

-- Zadanie 5

data Nat
    = S Nat
    | Z
  deriving Show

iter :: (a -> a) -> a -> Nat -> a
iter _ g Z = g
iter f g (S n) = f (iter f g n)

rec :: (Nat -> a -> a) -> a -> Nat -> a
rec f g = snd . iter (\(n, x) -> (S n, f n x)) (Z, g)

--------------------------------------------------------------------------------

-- Zadanie 6

tail :: [a] -> [a]
tail xs = foldr (\(_:tl) _ -> tl) [] [xs]

reverse :: [a] -> [a]
reverse = foldr (\x ys -> ys ++ [x]) []

zip :: [a] -> [b] -> [(a,b)]
zip = flip foldr (const []) $ \x f -> \case [] -> []
                                            (y:ys) -> (x, y) : f ys

--------------------------------------------------------------------------------

