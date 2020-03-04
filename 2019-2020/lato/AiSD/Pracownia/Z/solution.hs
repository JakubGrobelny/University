module Main where

import Data.List (sort)
import Control.Monad (mapM_)

main :: IO ()
main = do
    input <- sort . take 2 . map read . words <$> getLine
    printBetween (input !! 0) (input !! 1)
  where
    printBetween :: Int -> Int -> IO ()
    printBetween a b = mapM_ print [a .. b]