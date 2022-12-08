module Foo where

accum f [] = f []
accum f (x:xs) = f x + accum f xs

accum2 f (xs:xss) = f (head zs) + accum f zs
                    where
                        zs = xs ++ concat xss


data Tree a = Leaf a | Node (Tree a) (Tree a)

accumt f (Leaf n) = f n
accumt f (Node l r)  = accumt f l + accumt f r

unk f [] = f 0
unk f (x:xs) = unk (\n -> f (n+x)) xs
-- unk f (x:xs) = f x + unk f xs

gen :: Int -> [Tree a]
gen 0 = []
-- gen 1 = [Leaf ]
-- gen n = [Node l r | l <- gen [0..(n/2)-1], r <- gen[(n/2)..n]]4
gen n = [Node l r | k <- [0..nm],j <- [nm+1..n-1], l <- gen k, r <- gen j]
            where
                nm = div n 2

siev = [a |  [x | x<-[2..]] ]
        