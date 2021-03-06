-- Jakub Grobelny
-- Kurs języka Haskell
-- Lista 9, 18.05.2020

--------------------------------------------------------------------------------

{-# LANGUAGE TupleSections #-}

import Control.Monad.ST
import Data.Array.ST
import Control.Monad (forM_, forM, when)
import Data.Function (on)
import Data.Foldable (maximumBy, minimumBy)
import Data.List (sortBy)
import Data.STRef
import Data.Array
import GHC.Arr (freezeSTArray)

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

-- Zadanie 3

newtype Element s a = Element (STRef s (UFTree s a)) deriving Eq

data UFTree s a
    = Root (STRef s (Int, a))
    | Link a (Element s a)

makeSet :: a -> ST s (Element s a)
makeSet x = newSTRef . Root <$> newSTRef (0, x) >>= (Element <$>)

ufVal :: Element s a -> ST s a
ufVal (Element link) = do
    uftree <- readSTRef link
    case uftree of
        Root link -> do
            (_, val) <- readSTRef link
            return val
        Link val _ -> return val

find :: Element s a -> ST s (Element s a)
find element@(Element parentLink) = do
    parent <- readSTRef parentLink
    case parent of
        Root _ -> return element
        Link val parent' -> do
            representative <- find parent'
            writeSTRef parentLink (Link val representative)
            return representative

union :: Element s a -> Element s a -> ST s ()
union elem1 elem2 = do
    representative1@(Element link1) <- find elem1
    representative2@(Element link2) <- find elem2
    when (representative1 /= representative2) $ do
        Root reprLink1 <- readSTRef link1
        (rank1, val1) <- readSTRef reprLink1
        Root reprLink2 <- readSTRef link2
        (rank2, val2) <- readSTRef reprLink2
        if rank1 > rank2
            then do
                writeSTRef link2 (Link val2 elem1)
                writeSTRef reprLink1 (rank1 + 1, val1)
            else do
                writeSTRef link1 (Link val1 elem2)
                writeSTRef reprLink2 (rank2 + 1, val2)

type Weight = Int

type Vertex = Int

-- Uwaga: poprawny graf nieskierowany musi zawierać krawędzie w obie strony.
newtype Graph = Graph (Array Vertex [(Vertex, Weight)]) deriving Show

-- Funkcja tworząca graf nieskierowany
graphFromList :: (Int, Int) -> [(Vertex, Vertex, Weight)] -> Graph
graphFromList bounds edges = runST $ do
    array <- newArray bounds []
    forM_ edges $ \(v1, v2, w) -> do
        v1Edges <- readArray array v1
        v2Edges <- readArray array v2
        writeArray array v1 ((v2, w) : v1Edges)
        writeArray array v2 ((v1, w) : v2Edges)
    array <- freezeSTArray array
    return $ Graph array

-- np:
g = graphFromList (0,6) --
        [ (0,2,1)       --   (0)---1---(2)---2---(4)---9---(6)
        , (0,1,5)       --   |\         |         |
        , (2,3,3)       --   | \       /          |
        , (0,3,4)       --   5  \     3           7
        , (1,5,6)       --   |   4   /            |
        , (3,5,8)       --   |    \ /            /
        , (5,4,7)       --   |     (3)---8--\   /
        , (2,4,2)       --   |               \ /
        , (4,6,9) ]     --   (1)-------6-----(5)


preprocessEdges :: Graph -> ST s [(Element s Vertex, Element s Vertex, Weight)]
preprocessEdges (Graph graph) = do
    elemMapAssocs <- forM (assocs graph) $ \(vertex, _) -> do
        ufVertex <- makeSet vertex
        return (vertex, ufVertex)
    let elemArray = array (bounds graph) elemMapAssocs
        edges     = concatMap (splitEdges elemArray) (assocs graph)
    return $ sortBy (compare `on` weight) edges
  where
    weight (_, _, w) = w
    splitEdges :: Array Vertex (Element s Vertex)
               -> (Vertex, [(Vertex, Weight)])
               -> [(Element s Vertex, Element s Vertex, Weight)]
    splitEdges ufVertices (vertex, edges) = do
        (to, w) <- edges
        let to' = ufVertices ! to
            vertex' = ufVertices ! vertex
        return (vertex', to', w)

minSpanningTree :: Graph -> Graph
minSpanningTree graph = runST $ minSpanningTreeST graph
  where
    minSpanningTreeST :: Graph -> ST s Graph
    minSpanningTreeST graph@(Graph g) = do
        edges <- preprocessEdges graph
        array <- newArray (bounds g) []
        spanningTree <- minSpanningTree' array edges
        spanningTree' <- freezeSTArray spanningTree
        return . Graph $ spanningTree'
      where
        minSpanningTree' :: STArray s Vertex [(Vertex, Weight)]
                        -> [(Element s Vertex, Element s Vertex, Weight)] 
                        -> ST s (STArray s Vertex [(Vertex, Weight)])
        minSpanningTree' graph [] = return graph
        minSpanningTree' graph ((from, to, weight) : edges) = do
            reprFrom <- find from
            reprTo   <- find to
            when (reprFrom /= reprTo) $ do
                union from to
                fromVal <- ufVal from
                toVal   <- ufVal to
                edgesFrom <- readArray graph fromVal
                edgesTo   <- readArray graph toVal
                writeArray graph fromVal ((toVal, weight) : edgesFrom)
                writeArray graph toVal ((fromVal, weight) : edgesTo)
            minSpanningTree' graph edges

--------------------------------------------------------------------------------






