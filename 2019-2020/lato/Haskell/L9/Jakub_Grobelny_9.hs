-- Jakub Grobelny
-- Kurs języka Haskell
-- Lista 9, 18.05.2020

--------------------------------------------------------------------------------

{-# LANGUAGE TupleSections #-}

import Control.Monad.ST
import Data.Array.ST
import Control.Monad (forM_)
import Data.Function (on)
import Data.Foldable (maximumBy, minimumBy)

-- Zadanie 2

bucketSort :: [(Int, a)] -> [(Int, a)]
bucketSort [] = []
bucketSort xs = runST $ do
    buckets <- newArray (minKey, maxKey) [] :: ST s (STArray s Int [a])
    forM_ xs $ \(i, e) -> do
        bucket <- readArray buckets i
        writeArray buckets i (e : bucket)
    buckets' <- getAssocs buckets
    return $ concatMap appendBuckets buckets'
  where
    minKey = fst $ minimumBy (compare `on` fst) xs
    maxKey = fst $ maximumBy (compare `on` fst) xs
    appendBuckets :: (Int, [a]) -> [(Int, a)]
    appendBuckets (i, elems) = (i ,) <$> reverse elems

