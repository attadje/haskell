module Lists where 

map' :: (t -> a) -> [t] -> [a]
map' _ [] = []
map' f (x:xs) = f x : map' f xs


filter' :: Int -> [Int] -> [Int]
filter' _ [] = []
filter' a (x:xs) = if x < a then x : filter' a xs else filter' a xs


filter'' :: Int -> [Int] -> [Int]
filter'' a list = [x | x <- list,  x < a ]


filter''' :: Int -> [Int] -> [Int]
filter''' _ [] = []
filter''' a (x:xs)
    | x > a = x : filter''' a xs
    | otherwise = x : filter''' a xs


sum' :: [Int] -> Int
sum' [a] = a
sum' (x:xs) = x + sum' xs 


length' :: [Int] -> Int
length' [a] = 1
length' (x:xs) = 1 + length' xs


max' :: Ord a => [a] -> a
max' [a] = a
max' (x:xs) = max x $ max' xs


evenFilter :: Integral a => [a] -> [a]
evenFilter [] = []
evenFilter (x:xs) = if even x then x : evenFilter xs else evenFilter xs


evenFilter'' :: Integral a => [a] -> [a]
evenFilter'' [] = []
evenFilter'' (x:xs)
    | even x = x : evenFilter'' xs
    | otherwise = evenFilter'' xs


evenFilter''' :: Integral a => [a] -> [a]
evenFilter''' xs = [x' | x' <- xs, even x']


evenFilter'''' :: (a -> Bool) -> [a] -> [a]
evenFilter'''' p (x:xs) = [x' | x' <- xs, p x']


addOneToAll :: [Int] -> [Int]
addOneToAll [] = [] 
addOneToAll (x:xs) = x + 1 : addOneToAll xs


absAll :: Num a => [a] -> [a]
absAll [] = []
absAll (x:xs) = abs x : absAll  xs


sqrtAll :: (Num a) => [a] -> [a]
sqrtAll [] = []
sqrtAll (x:xs) = x ^ 2 : sqrtAll xs


mapInt :: (t -> a) -> [t] -> [a]
mapInt _ [] = []
mapInt f (x:xs) = f x : mapInt f xs


addOneToAll', absAll', sqrtAll', addOneToAll''  :: [Int]
addOneToAll' = mapInt (+1) [2,3,4,5,5]
absAll' = mapInt abs [2,3,4,5,5] 
sqrtAll' = mapInt (^2) [2,3,4,4]
addOneToAll'' = mapInt (\x -> x + 1) [1..10]


keepOnlyPositive :: [Int] -> [Int]
keepOnlyPositive xs = [x | x <- xs, x > 0]


keepOnlyPositive'' :: (Ord a, Num a) => [a] -> [a]
keepOnlyPositive'' [] = []
keepOnlyPositive'' (x:xs) 
    | x > 0 = x : keepOnlyPositive'' xs
    | otherwise = keepOnlyPositive'' xs


keepOnlyEvenNb :: Integral a => [a] -> [a]
keepOnlyEvenNb [] = []
keepOnlyEvenNb (x:xs) 
    | even x = x : keepOnlyEvenNb xs
    | otherwise = keepOnlyEvenNb xs


keepOnlyEvenNb'' :: Integral a => [a] -> [a]
keepOnlyEvenNb'' xs = [x | x <- xs, even x]


filterInt :: (a -> Bool) -> [a] -> [a]
filterInt f xs = [x | x <- xs, f x]


filterInt'' :: (a -> Bool) -> [a] -> [a]
filterInt'' _ [] = []
filterInt'' f (x:xs)
 | f x = x : filterInt'' f xs
 | otherwise = filterInt'' f xs


sumList :: Num a => [a] -> a
sumList [a]  = a
sumList (x:xs) = x + sumList xs 

productList :: Num a => [a] -> a
productList [a] = a
productList (x:xs) = x * productList xs   


maxList :: Ord a => [a] -> a
maxList [a] = a
maxList (x:xs) = max x $ maxList xs 


head' :: Eq a => [a] -> Maybe a
head' xs 
    | xs == [] = Nothing
    | otherwise = Just (head xs) 

head'' :: [a] -> Maybe a
head'' [] = Nothing
head'' (x:_) = Just x

addOne :: [Int] -> Maybe Int
addOne [] = Nothing
addOne (x:xs) = Just (1 + x)

addMap'' :: (a -> b) -> Maybe a -> Maybe b
addMap'' f Nothing = Nothing
addMap'' f (Just x) = Just (f x)

addOne'' :: Num b => [b] -> Maybe b
addOne'' l = addMap'' (+1) (head'' l)

everySecondElem :: [a] -> [a]
everySecondElem [] = []
everySecondElem [a] = [a]
everySecondElem [a,b] = [b]
everySecondElem (a:b:xs) = b: everySecondElem xs 

factorial' :: Int -> Int
factorial' 0 = 1
factorial' x = x * factorial' (x-1) 

foldr' :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
foldr' _ acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

lentghFldr :: Num a => [a] -> a
lentghFldr = foldr' (\cv nv -> 1 + nv ) 0 

sumFldr :: [Int] -> Int
sumFldr xs = foldr' (\cv nv -> cv + nv) 0 xs

productFldr :: [Int] -> Int
productFldr xs = foldr' (\cv nv -> cv * nv) 1 xs

factorialFldr :: (Num a, Enum a) => a -> a
factorialFldr nb = foldr' (\cv nv -> cv * nv) 1 [1..nb]

filterFldr :: (a -> Bool) -> [a] -> [a]
filterFldr f = foldr' (\cv -> if (f cv) then (cv:) else id) [] 

mapFldr :: (a -> a) -> [a] -> [a]
mapFldr f xs = foldr' (\cv nv -> f cv : nv) [] xs 




        
 
        















 

































