data BTree a = Leaf | Node (BTree a) a (BTree a) deriving Show

dfnum :: BTree a -> BTree Integer
dfnum t = head (forestNum 1 [t])
    where
        forestNum :: Integer -> [BTree a] -> [BTree Integer]
        forestNum n [] = []
        forestNum n (Leaf : ts) =
            Leaf : forestNum n ts
        forestNum n (Node l _ r : ts) =
            Node l' n r' : ts' 
                where
                    (l' : r' : ts') = forestNum (n + 1) $ l : r : ts

bfnum :: BTree a -> BTree Integer
bfnum t = head $ bfnum' 1 [t]
    where
        numLayer n [] [] = (n, [], [])
        numLayer n (Leaf : ts) nts =
            (m, Leaf : nts, cs)
                where
                    (m, nts, cs) = numLayer n ts nts
        numLayer n (Node l _ r : ts) ncs' =
            (m, Node nl n nr : nts, l : r : cs)
                where 
                    (m, nts, cs) = numLayer (n+1) ts ncs
                    (nl : nr : ncs) = ncs'
        bfnum' _ [] = []
        bfnum' n ts = nts
            where
                (m, nts, cs) = numLayer n ts $ bfnum' m cs
