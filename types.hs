module Foo where

-- type Money = Int

-- jos :: (Money -> k) -> k
data Nat = Zero | Succ Nat

nxt :: Int -> Nat
nxt 0 = Zero
nxt x = Succ (nxt (x-1))

prv :: Nat -> Int
prv Zero = 0
prv (Succ x) = 1 + prv x


ad :: Nat -> Nat -> Nat 
ad Zero     n = n
ad (Succ x) y = Succ (ad x y)

mlt :: Nat -> Nat -> Nat
mlt Zero _ = Zero
mlt (Succ x) y = ad (mlt x y) y

data Tree a = Leaf a | Node (Tree a) (Tree a)