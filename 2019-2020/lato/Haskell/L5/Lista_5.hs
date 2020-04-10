{-|
Module      : Lista_5.hs
Copyright   : Jakub Grobelny
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MonoLocalBinds #-}

module Lista_5 where

import Control.Monad

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


-- Zadanie 2
subseqM :: MonadPlus m => [a] -> m [a]
subseqM [] = return mzero
subseqM (x:xs) = do
    ys <- subseqM xs
    return (x:ys) `mplus` return ys

ipermM :: MonadPlus m => [a] -> m [a]
ipermM [] = return mzero
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
    zs <- spermM ys
    return (y:zs)
  where
    select :: MonadPlus m => [a] -> m (a, [a])
    select []     = undefined
    select [x]    = return (x, [])
    select (x:xs) = return (x, xs) `mplus` do
        (y, ys) <- select xs
        return (y, x:ys)

-- Zadanie 6

