module Foo where
import Data.Char

-- this applies a fxn to all elements of a list, and the sums up the list
accum f [] = f []
accum f (x:xs) = f x + accum f xs

-- this concats a list of lists in to a list, applies a fx to all its elems and sums em' all up 
accum2 f (xs:xss) = f (head zs) + accum f zs
                    where
                        zs = xs ++ concat xss


data Tree a = Leaf a | Node (Tree a) (Tree a)

-- this applies a fxn to the elems in all the Leaves of a tree, and then sums em' all up
accumt f (Leaf n) = f n
accumt f (Node l r)  = accumt f l + accumt f r

-- this applies a fxn to the sum of a list
unk f [] = f 0
unk f (x:xs) = unk (\n -> f (n+x)) xs
-- unk f (x:xs) = f x + unk f xs

-- generates a tree of a givensize n
gen :: Int -> [Tree a]
gen 0 = []
-- gen 1 = [Leaf ]
-- gen n = [Node l r | l <- gen [0..(n/2)-1], r <- gen[(n/2)..n]]4
gen n = [Node l r | k <- [0..nm],j <- [nm+1..n-1], l <- gen k, r <- gen j]
            where
                nm = div n 2

-- gives back the infinite list of prime numbers
primes = sieve [2..]
-- uses the sieve of Erathostenes to calculate the numbers
sieve (p:xs) = p:sieve [a | a <- xs, a `mod` p /= 0] 

-- checks if a tupple of two prime numbers are twins. twins are prime numbers which only differ by two
twin (x,y) = y == x+2
-- gives out the list of tupples of all twin prime numbers. Note that it has yet to be proven, whether this list is infinite or not!
twins = filter twin (zip primes (tail primes))

-- checks if two lists, more specifically lists of characters are equal, whether they're the same letter case or not. 
eqls xs ys = if length xs == length ys then (and [toLower (xs!!i) == ys!!i || toLower (ys!!i) == xs!!i || xs!!i == ys!!i |  i<-[0..(length xs)-1]])
                else False



type Audi = String
type Zero = String

data  Money = Zero
data Car = Audi deriving Show

getCar :: (Money -> Car) -> Car
getCar f = f Zero


zp xs ys = [(x,y)| x<-xs, y<-ys]
zp' xs ys = [((xs)!!i,(ys)!!i) | i<-[0..z]]
            where z = min (length xs-1) (length ys-1)