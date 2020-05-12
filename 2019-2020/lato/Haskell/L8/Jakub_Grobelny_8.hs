-- Jakub Grobelny
-- Kurs języka Haskell
-- Lista 8, 8.05.2020

--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}

-- Zadanie 1

data Nat
    = Zero
    | Succ Nat

data Tree :: Nat -> * -> * where
    Leaf :: Tree Zero a 
    Node :: Tree h a -> Tree h a -> a -> Tree (Succ h) a

tree = Node (Node Leaf Leaf 'a') (Node Leaf Leaf 'b') 'c'

--------------------------------------------------------------------------------


