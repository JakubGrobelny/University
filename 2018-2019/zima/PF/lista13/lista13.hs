import Prelude hiding ((++), head, tail, length, null, (!!))
import qualified Prelude ((++), head, tail, length, null, (!!))

-- zadanie 1

class List l where
    nil :: l a
    cons :: a -> l a -> l a
    head :: l a -> a
    tail :: l a -> l a
    (++) :: l a -> l a -> l a
    (!!) :: l a -> Int -> a
    toList :: [a] -> l a
    fromList :: l a -> [a]

instance List [] where
    nil = []
    cons x xs = x : xs
    head (x:_) = x
    head [] = error "head: empty list!"
    tail (_:xs) = xs
    tail [] = error "tail: empty list!"
    (++) [] ys = ys
    (++) (x:xs) ys = x : (xs ++ ys)
    (!!) [] _ = error "!!: empty list"
    (!!) (x:_) 0 = x
    (!!) (_:xs) n = xs !! (n - 1)
    toList xs = xs
    fromList xs = xs

-- zadanie 2

class List l => SizedList l where
    length :: l a -> Int
    null :: l a -> Bool
    null l = length l == 0

instance SizedList [] where
    length [] = 0
    length (_:xs) = 1 + length xs
    null [] = True
    null _ = False

-- zadanie 3

data SL l a = SL { len :: Int, list :: l a }

instance List l => List (SL l) where
    nil = SL 0 nil
    cons x xs = SL (1 + len xs) (cons x $ list xs)
    head xs = head $ list xs
    tail xs = SL (len xs - 1) (tail $ list xs)
    (++) xs ys = SL (len xs + len ys) (list xs ++ list ys)
    (!!) xs n = list xs !! n
    toList xs = SL (length xs) (toList xs) where
        length :: [a] -> Int
        length [] = 0
        length (_:xs) = 1 + length xs
    fromList xs = fromList $ list xs

instance List l => SizedList (SL l) where
    length xs = len xs

-- zadanie 4

infixr 6 :+
data AppList a = 
      Nil
    | Sngl a
    | AppList a :+ AppList a

instance Show a => Show (AppList a) where
    show xs = "[" ++ show' xs ++ "]" where
        show' :: Show a => AppList a -> String
        show' Nil = ""
        show' (Sngl a) = show a
        show' (xs :+ Nil) = show' xs
        show' (xs :+ ys) = show' xs ++ ", " ++ show' ys

instance List AppList where
    nil = Nil
    cons x xs = (Sngl x) :+ xs
    head Nil = error "head: empty list!"
    head (Sngl a) = a
    head (xs :+ ys) = head xs
    tail Nil = error "tail: empty list!"
    tail (Sngl _) = Nil
    tail (xs :+ ys) = ys
    (++) Nil ys = ys
    (++) (x :+ xs) ys = x :+ (xs ++ ys)
    (!!) Nil _ = error "!!: empty list"
    (!!) ((Sngl x) :+ _) 0 = x
    (!!) ((Sngl _) :+ xs) n = xs !! (n - 1)
    toList [] = Nil
    toList (x:xs) = (Sngl x) :+ toList xs
    fromList Nil = []
    fromList ((Sngl x) :+ xs) = x : fromList xs

instance SizedList AppList where
    length Nil = 0
    length (Sngl _) = 1
    length (xs :+ ys) = length xs + length ys

-- zadanie 5

newtype DiffList a = DL ([a] -> [a])

instance List DiffList where
    nil = DL id
    cons x (DL xs) = DL (\ys -> x : xs ys)
    head (DL xs) = head (xs [])
    tail (DL xs) = DL (\ys -> tail $ xs ys)
    (++) (DL xs) (DL ys) = DL (\zs -> xs (ys zs))
    (!!) (DL xs) n = xs [] !! n
    toList xs = DL (\zs -> xs ++ zs)
    fromList (DL xs) = xs []

instance SizedList DiffList where
    length (DL xs) = length $ xs []
    null (DL xs) = case xs [] of
        [] -> True
        _ -> False

instance Show a => Show (DiffList a) where
    show (DL xs) = show $ xs []