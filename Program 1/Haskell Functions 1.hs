-- Name		: Dylan Jones
-- Project	: Haskell Functions (Homework 4)
-- Date		: 3/6/2012

-- Import libraries for problem 4
import Char

-- Problem 1
isPrime :: Integer -> Bool
isPrime num = if length (factors num) < 2 then True else False
	where
		factors num = [x| x <- [1..(num-1)], num `mod` x == 0]

-- Problem 2
primes :: Integer -> [Integer]
primes num = factors [2..num]
	where
		factors [] = []
		factors (x:xs) = x : factors [y| y <- xs, y `mod` x > 0]

-- Problem 3
-- Set
data Set a = Set [a] deriving(Show,Eq,Ord)

-- empty list
emptySet :: Set a
emptySet = Set []

-- setAdd
setAdd :: (Eq a) => a -> Set a -> Set a
setAdd x (Set xs)
	| not (x `elem` xs) = Set (x:xs)
	| otherwise			= Set xs
	
-- setUnion
setUnion (Set xs) (Set ys) = Set ([x| x <- xs, elem x ys == False] ++ ys)

-- setIntersection
setIntersection :: (Eq a) => Set [a] -> Set [a] -> Set [a]
setIntersection (Set xs) (Set ys) = Set [x| x <- xs, y <- ys, x == y]

-- Problem 4
allCaps :: [Char] -> [Char]
allCaps xs = map toUpper xs

-- Problem 5
begList :: Integer -> [[Integer]]
begList num = map fun [x| x <- [0..num-1]]
	where
		fun size = [1..size]

		-- Problem 6
summation :: Integer -> Integer -> (Integer -> Integer) -> Integer
summation lower upper f = sum xs
	where
		xs = map f [lower..upper]

-- Problem 7
prods :: Integer -> [Integer]
prods num = products [num..]
	where
		products xs = [y * num| y <- xs]

-- Problem 8
isPalendrome :: (Eq a) => [a] -> Bool
isPalendrome [] = True
isPalendrome (x:[]) = True
isPalendrome (x:xs) 
	|(x `elem` (reverse xs)) = True
	|otherwise = False
	
-- Problem 9
flatten :: [[Integer]] -> [Integer]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

-- Problem 10
compress :: [Char] -> [Char]
compress [] = []
compress (x:[]) = [x]
compress (x:xs)
	| x == head xs = compress xs
	| otherwise = [x] ++ compress xs

-- Problem 11
pack :: [Char] -> [[Char]]
pack [] = []
pack (x:xs) = pack' x xs [x]
pack' c [] a = a:[]
pack' c (x:xs) ys 
	| c == x = (pack' x xs (x:ys))
	| otherwise = ys: pack' x xs [x]