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
import Data.List (sortBy)

-- Zadanie 2

bucketSort :: [(Int, a)] -> [(Int, a)]
bucketSort [] = []
bucketSort xs = runST $ do
    buckets <- newArray (minKey, maxKey) [] :: ST s (STArray s Int [a])
    forM_ xs $ \(i, e) -> do
        bucket <- readArray buckets i
        writeArray buckets i (e : bucket)
    buckets' <- getAssocs buckets
    return $ concatMap (\(i, elems) -> (i ,) <$> reverse elems) buckets'
  where
    minKey = fst $ minimumBy (compare `on` fst) xs
    maxKey = fst $ maximumBy (compare `on` fst) xs

-- Sortowanie 1000000 elementów:
-- bucketSort:
--    real    0m7,482s
--    user    0m7,171s
--    sys     0m0,296s
-- sortBy (compare `on` fst):
--    real    0m10,580s
--    user    0m10,195s
--    sys     0m0,293s
