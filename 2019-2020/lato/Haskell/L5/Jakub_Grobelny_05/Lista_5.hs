{-|
Module      : Lista_5
Copyright   : (c) Jakub Grobelny
-}

-------------------------------------------------------------------------------- 

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}

module Lista_5 where

import Control.Monad
import Control.Applicative

-------------------------------------------------------------------------------- 

-- Zadanie 1

class NFData a where
    rnf :: a -> ()

instance Num a => NFData a where
    rnf !n = n `seq` ()

instance NFData a => NFData [a] where
    rnf ![] = ()
    rnf (!x:(!xs)) = rnf x `seq` rnf xs

instance (NFData a, NFData b) => NFData (a, b) where
    rnf (!a, !b) = rnf a `seq` rnf b

deepseq :: NFData a => a -> b -> b
deepseq !a !b = rnf a `seq` b

($!!) :: NFData a => (a -> b) -> a -> b
($!!) !f !x = x `deepseq` f $! x

-------------------------------------------------------------------------------- 

-- Zadanie 2

subseqM :: MonadPlus m => [a] -> m [a]
subseqM []     = return mzero
subseqM (x:xs) = do
    ys <- subseqM xs
    return (x:ys) `mplus` return ys

ipermM :: MonadPlus m => [a] -> m [a]
ipermM []     = return mzero
ipermM (x:xs) = ipermM xs >>= insert x
  where
    insert :: MonadPlus m => a -> [a] -> m [a]
    insert x [] = return [x]
    insert x ys'@(y:ys) = return (x:ys') `mplus` do
        zs <- (y :) <$> insert x ys
        return zs

spermM :: MonadPlus m => [a] -> m [a]
spermM [] = return mzero
spermM xs = do
    (y,ys) <- select xs
    zs     <- spermM ys
    return (y:zs)
  where
    select :: MonadPlus m => [a] -> m (a, [a])
    select []     = undefined
    select [x]    = return (x, [])
    select (x:xs) = return (x, xs) `mplus` do
        (y, ys) <- select xs
        return (y, x:ys)

-------------------------------------------------------------------------------- 

-- Zadanie 6

data List t a
    = Cons a (t a)
    | Nil

newtype SimpleList a = SimpleList { fromSimpleList :: List SimpleList a }

class ListView t where
    viewList :: t a -> List t a
    toList :: t a -> [a]
    cons :: a -> t a -> t a
    nil :: t a
    toList (viewList -> Cons x xs) = x : toList xs
    toList (viewList -> Nil) = []

data CList a 
    = CList a :++: CList a
    | CSingle a
    | CNil
  deriving Show

instance ListView CList where
    viewList (CSingle a) = Cons a CNil
    viewList (CNil) = Nil
    viewList (CNil :++: ys) = viewList ys
    viewList (CSingle a :++: ys) = Cons a ys
    viewList (xs :++: ys :++: zs) = 
        case viewList xs of
            Nil -> viewList $ ys :++: zs
            Cons x xs' -> Cons x (xs' :++: ys :++: zs)
    cons x xs = CSingle x :++: xs
    nil = CNil

instance Functor CList where
    fmap f CNil = CNil
    fmap f (CSingle a) = CSingle $ f a
    fmap f (xs :++: ys) = fmap f xs :++: fmap f ys

instance Applicative CList where
    pure x = CSingle x
    CSingle f    <*> xs = fmap f xs
    CNil         <*> xs = CNil
    (xs :++: ys) <*> zs = (xs <*> zs) :++: (ys <*> zs)

instance Monad CList where
    return = pure
    CNil         >>= _ = CNil
    (CSingle x)  >>= f = f x
    (xs :++: ys) >>= f = (xs >>= f) :++: (ys >>= f)

-- Bez instancji Alternative kompilator narzeka gdy implementuje się MonadPlus
instance Alternative CList where
    empty = CNil
    (<|>) = (:++:)

instance MonadPlus CList where
    mzero = CNil
    mplus = (:++:)

instance Foldable CList where
    foldr _ n CNil = n
    foldr f n (CSingle x) = f x n
    foldr f n (xs :++: ys) = foldr f (foldr f n ys) xs

instance Traversable CList where
    traverse _ CNil = pure CNil
    traverse f (CSingle x) = CSingle <$> f x
    traverse f (xs :++: ys) = liftA2 (:++:) (traverse f xs) (traverse f ys)

-------------------------------------------------------------------------------- 

-- Zadanie 7

newtype DList a = DList { fromDList :: [a] -> [a] }

dappend :: DList a -> DList a -> DList a 
dappend xs ys = DList $ \tl -> fromDList xs (fromDList ys tl)

instance ListView DList where
    cons x xs = DList $ \tl -> x : fromDList xs tl
    nil = DList id
    viewList    (flip fromDList [] -> [])    = Nil
    viewList xs@(flip fromDList [] -> (h:_)) = Cons h xs'
      where
        xs' = DList $ \tl -> tail $ fromDList xs tl
    toList = flip fromDList []

instance Functor DList where
    fmap f (DList xs) = DList $ \tl -> (f <$> xs []) ++ tl

instance Applicative DList where
    pure x = DList $ \tl -> x : tl
    (DList xs) <*> (DList ys) = DList $ \tl -> (xs [] <*> ys []) ++ tl

instance Monad DList where
    return = pure
    (DList xs) >>= f = DList $ \tl -> 
        fromDList (foldr dappend (DList id) xss) $ tl
      where
        xss = f <$> xs []

instance Alternative DList where
    empty = nil
    (<|>) = dappend

instance MonadPlus DList where
    mzero = nil
    mplus = dappend

instance Foldable DList where
    foldr f n (DList xs) = foldr f n $ xs []

instance Traversable DList where
    traverse f (DList xs) = DList . (++) <$> traverse f (xs [])

-------------------------------------------------------------------------------- 
