module Foo where
-- adds two integers together
ad :: Int ->  (Int -> Int)
ad x y = x + y

-- multiplies two integers together
mlt :: Int -> (Int -> (Int -> Int))
mlt x y z = x * y * z

-- zips two lists together into a list of tupples consisting of the two previous list
zp :: [a] ->  [b] -> [(a,b)]
zp [] [] = []
zp [] zs = []
zp zs [] = []
zp (x:xs) (y:ys) = (x,y): zp xs ys

-- takes the first number of elemets stipulated by x from a list, and gives back a list of those elements
tk :: Int -> [a] -> [a]
tk x zs = [a | a <- fst (unzip (zip zs [1..x]))]

-- experimenting what unzip does
-- zz :: [(a,b)] -> [a]
-- zz xs = [x | x <- fst xs]
zz [] = []
zz ((x,y):xs) = x:zz xs