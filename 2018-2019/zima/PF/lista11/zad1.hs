ana :: (b -> Maybe (a, b)) -> b -> [a]
ana f st = case f st of
    Nothing -> []
    Just (v, st') -> v : ana f st'

cata :: (a -> b -> b) -> b -> [a] -> b
cata f v [] = v
cata f v (x:xs) = f x (cata f v xs)


zip :: [a] -> [b] -> [(a, b)]
zip xs ys = ana f (xs, ys)
    where
        f :: ([a], [b]) -> Maybe ((a, b), ([a], [b]))
        f ([], _) = Nothing
        f (_, []) = Nothing
        f (x:xs, y:ys) = Just ((x, y), (xs, ys))

iterate :: (a -> a) -> a -> [a]
iterate f start = ana (\st -> Just (st, f st)) start

map :: (a -> b) -> [a] -> [b]
map f xs = ana g xs
    where
        g [] = Nothing
        g (x:xs) = Just (f x, xs)


length :: [a] -> Integer
length xs = cata (\_ res -> 1 + res) 0 xs

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = cata (\elem res -> if p elem then elem : res else res) [] xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = cata (\elem res -> f elem : res) [] xs


data Expr a b =
      Number b
    | Var a
    | Plus (Expr a b) (Expr a b)
    deriving Show

data Either3 a b c =
      Left' a
    | Middle' b
    | Right' c

anaExpr :: (c -> Either3 a b (c, c)) -> c -> Expr b a
anaExpr f st = case f st of
    Left' num -> Number num
    Middle' var -> Var var
    Right' (l, r) -> Plus (anaExpr f l) (anaExpr f r)

cataExpr :: (a -> c, b -> c, c -> c -> c) -> Expr a b -> c
cataExpr f@(lookup, id, (·)) expr = case expr of
    Number n -> id n
    Var x -> lookup x
    Plus a b -> (cataExpr f a) · (cataExpr f b)

eval :: Num b => Eq a => [(a , b)] -> Expr a b -> b
eval env expr = cataExpr (lookup, id, (+)) expr
    where
        lookup var = snd $ head $ Main.filter ((== var) . fst) env
