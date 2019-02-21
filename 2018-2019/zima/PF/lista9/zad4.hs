iperm :: [a] -> [[a]]
iperm [] = [[]]
iperm (x:xs) = concatMap (insert x) (iperm xs)
    where
        insert :: a -> [a] -> [[a]]
        insert x [] = [[x]]
        insert x (y:ys) = (x:y:ys) : map (y:) (insert x ys)

-- sperm :: [a] -> [[a]]
-- sperm [] = []
-- sperm [x] = [[x]]
-- sperm xs =
--     [y:zs | (y,ys) <- select xs, zs <- sperm ys]
--     where
--         select :: [a] -> [(a, [a])]
--         select [x] = [(x, [])]
--         select (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- select xs]