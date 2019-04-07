module Main where

solve :: (Integer, Integer) -> IO [()]
solve (a, b) = if a > b
    then solve (b, a)
    else mapM print [a..b]

main :: IO [()]
main = do
    input <- getLine
    let (a, b) = unwrap $ map (read :: String -> Integer) (words input)
    solve (a,b)
    where
        unwrap :: [a] -> (a, a)
        unwrap xs = (xs !! 0, xs !! 1)