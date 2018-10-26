--Los ejercicios que faltan del EJ1 son los que entregue la semana anterior

--EJERCICIO 1:

ap10 = [i * (-1) ^ (i+1) | i <- [1..]]
ap11 = [0] ++ baraja [1..] [-1,-2..]

baraja (x:xs) []     = xs
baraja [] (y:ys)     = ys
baraja (x:xs) (y:ys) = x:y:baraja xs ys

--EJERCICIO 2:
filter2 ::  [a] -> (a -> Bool) -> (a -> Bool) -> ([a], [a])
filter2 xs p q = ([x | x <- xs, p x], [x | x <- xs, q x])

filters :: [a] -> [(a -> Bool)] -> [[a]]
filters xs ps = [[x | x <- xs, p x] | p <- ps]

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not.p) xs)

span' ::  (a -> Bool) -> [a] -> ([a], [a])
span' p xs = (takeWhile p xs, dropWhile p xs)

mapx :: a -> [(a -> b)] -> [b]
mapx x [] = []
mapx x (f:fs) = (f x):mapx x fs

iguales :: (Enum a, Eq b) => (a -> b) -> (a -> b) -> a -> a -> Bool
iguales f g n m = foldr (&&) True [f x == g x | x <- [n..m]]

cuantos :: (a -> Bool) -> [a] -> Int
cuantos p [] = 0
cuantos p (x:xs)
 | p x       = 1 + cuantos p xs
 | otherwise = cuantos p xs

mayoria :: (a -> Bool) -> [a] -> Bool
mayoria p xs
 | cuantos p xs >= div (length xs) 2 = True
 | otherwise = False

par :: Int -> Bool --Para probar
par x
 | mod x 2 == 0 = True
 | otherwise    = False




--EJERCICIO 6:
prefixes, suffixes, sublists, perms, inits  :: [a] -> [[a]]
prefixes xs = [take n xs | n <- [0..length xs]]
suffixes xs = [drop n xs | n <- [0..length xs]]
sublists xs = prefixes xs ++ suffixes xs
parts xs = nub (foldr (++) [] [prefixes y | y <- suffixes xs])
perms xs0 = xs0 : perms' xs0 []
  where
    perms' [] _ = []
    perms' (x:xs) ys = foldr intercalar (perms' xs (x:ys)) (perms ys)
      where intercalar    xs     r = let (_,zs) = intercalar' id xs r in zs
            intercalar' _ []     r = (xs, r)
            intercalar' f (y:ys) r = let (us,zs) = intercalar' (f . (y:)) ys r
                                     in  (y:us, f (x:y:us) : zs)
inits xs = [take n xs | n <- [0..(length xs - 1)]]


--Funciones de List
nub :: (Eq a) => [a] -> [a]
nub l                  = nub' l []
  where
    nub' [] _          = []
    nub' (x:xs) ls
        | x `elem` ls  = nub' xs ls
        | otherwise    = x : nub' xs (x:ls)

