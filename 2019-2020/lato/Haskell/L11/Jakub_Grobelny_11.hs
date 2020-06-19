-- Jakub Grobelny
-- Kurs języka Haskell
-- Lista 11, 19.06.2020

{-# LANGUAGE PolyKinds, 
             TypeOperators, 
             MultiParamTypeClasses, 
             FunctionalDependencies,
             RankNTypes
#-}

import Control.Monad((>=>))

--------------------------------------------------------------------------------

-- Zadanie 1

data Yoneda f a = Yoneda (forall x . (a -> x) -> f x)

instance Functor (Yoneda f) where
    fmap f (Yoneda g) = Yoneda $ g . (. f)

toYoneda :: (Functor f) => f a -> Yoneda f a
toYoneda m = Yoneda (<$> m)

fromYoneda :: Yoneda f a -> f a
fromYoneda (Yoneda f) = f id

--------------------------------------------------------------------------------

class Category (t :: k -> k -> *) where
    ident :: a `t` a
    comp  :: b `t` c -> a `t` b -> a `t` c

instance Category (->) where
    ident = id
    comp  = (.)

-- Zadanie 5

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance (Monad m) => Category (Kleisli m) where
    ident = Kleisli return
    (Kleisli f) `comp` (Kleisli g) = Kleisli (g >=> f)

--------------------------------------------------------------------------------