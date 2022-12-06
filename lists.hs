module Foo where
-- surt sorts a list
surt [] = []
surt (x:xs) = surt ys ++ [x] ++ surt zs
            where
              ys = [a | a <- xs, a <= x]
              zs = [b | b <- xs, b > x]

double x = x + x
qtrpl x = double (double x)

-- share' does all posible permutations between two lists and gives back a list of tupels of those permutations
share' :: [a] -> [a] -> [(a, a)]
share' xs ys = [(x,y) | x <- xs, y <- ys]

-- con' cancatenates a list of lists 
con' :: [[a]] -> [a]
con' xss = [x | xs <- xss, x <- xs]

-- paires adjescent elements of a list together and gives back a list of tuples
adjesc xs = zip xs (tail xs)

-- checks wether a list is sorted 
sorted' xs = and [x <= y | (x,y) <- adjesc xs]

-- gets the position of something in a list
pos x xs = [b | (a,b) <- zip xs [0..], a == x]

-- counts the nu of times something appears in a list
cnt x xs = length [a | a <- xs, a == x]


-- pyth searches for triples of a certain number that satisfies pythagora's theorem
pyth :: Int -> [(Int,Int,Int)]
pyth x = [a | (i,j) <- [b | b <- adjesc [1..x]], i^2 + j^2 == x^2, a <- [(i,j,x)]]
--          zs = [(snd ys, fst ys, x)]

-- prfct checks if a number is a perfect number
prfct :: Int -> Bool
prfct  x = sum[b | b <- [1..x-1], mod x b == 0] == x
-- also::  prct x = sum (init (factors x)) == x
--                  sum (factors x) - x == x

-- list of perfect numbers up to a given limit
perfects :: Int -> [Int]
perfects x = [a | a <- [1..x], sum [b | b <- [1..a-1], mod a b == 0] == a]
-- also::  perfects x = [a | a <- [1..x], prfct a]

-- sclar does the scalar product of two lists of integers of length n. gives back the sum of the products of the corresponding integers
sclar :: [Int] -> [Int] -> Int
sclar xs ys = sum [x*y | (x,y) <- zip xs ys] -- v1: simplest
--sclar xs ys = sum [xs!!i * ys!!i | i <- [0..(length xs)-1]] -- v2: assuming both lists have the same length
--sclar xs ys = sum [xs!!i * ys!!i | i <- [0..n-1]] -- v3
  --            where
    --            n = if length xs <= length ys then length xs else length ys --assuming they aren't the same length

-- hd gets the head of a list
hd (x:xs) = x -- also hd xs = xs!!0
-- tl gets the last elem of a list
tl xs = hd(reverse xs) -- tl xs = xs!!(length xs-1)
-- tll gets the rest of a list
tll xs = [xs!!i | i <- [1..(length xs-1)]] -- also:: tll xs = drop 1 xs

-- drp, drp', drp'' and drp''' drops the first n elements of a list and gives back the rest
drp n xs = reverse [ [zs | zs<- reverse xs]!!i | i <-[0..(length xs-(n+1))] ] -- slowest!! by faaaaar... took an average of 1968 sec to drop 19999 from 100000
drp' n xs = reverse tk
            where
            tk = [ys!!i | i<- [0..length ys- (n+1)]]
            ys = reverse xs  -- took an average of 27 sec to drop 19999 from 100000
drp'' n xs = reverse (take (length xs-n) (reverse xs)) -- most memory efficient!!. took an average of 7 sec to drop 19999 from 100000
drp''' n xs = reverse (tk (length xs-n) zs)
              where
                tk a as = [as!!i | i<- [0..a-1]]
                zs = reverse xs  -- took an average of 46 sec to drop 19999 from 100000

-- hlv takes a list and gives back a tupple of lists of two halves of that list
hlv :: [a] -> ([a],[a])
hlv xs = (ys, zs)
          where
            l = length xs
            d = div l 2
            ys = [xs!!i | i<- [0..d-1]]
            zs = [xs!!i | i<- [d..(l-1)]]