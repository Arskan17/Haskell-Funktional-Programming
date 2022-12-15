module Foo where

data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node l r) = Node (fmap g l) (fmap g r)


data Mayb a = Nothng | Jst a

instance Functor Mayb where
    -- fmap :: (a -> b) -> Mayb a -> Mayb b
    fmap g Nothng = Nothng
    fmap g (Jst x) = Jst (g x)


instance Functor [] where
    -- fmap :: (a -> b) -> [a] -> [b]
    fmap g [] = []
    fmap g (x:xs) = (g x):(fmap g xs)

instance Functor g where
    -- fmap :: (a -> b) -> g a -> g b
    fmap g x = g x

instance Applicative Tree where
    -- pure :: a -> Tree a
    pure x = Leaf x
    -- (<*>) :: Tree (a -> b) -> Tree a -> Tree b
    (<*>) (Node g h) (Node l r) = Node (fmap g (Node l r)) (fmap h (Node l r))

instance Applicative Mayb where
    -- pure :: a -> Mayb a
    pure x = Jst x
    -- (<*>) :: Mayb (a -> b) -> Mayb a -> Mayb b
    (<*>) Nothing _ = Nothing
    (<*>) (Jst g) mx = fmap g mx

instance Applicative [] where
    -- pure :: a -> [a]
    pure x = [x]
    -- (<*>) :: [(a -> b)] -> [a] -> [b]
    (<*>) fs xs = [f x | f <- fs, x <- xs] 
    -- (<*>) [] (x:xs) = []
    -- (<*>) (f:fs) (x:xs) = (f x):(fs <*> xs)
