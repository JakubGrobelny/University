module Automaton where

import Data.Map

data Automaton = Automaton
    { transitions :: Map Int (Char -> Int)
    , accepting   :: [Int]
    , initial     :: Int
    }