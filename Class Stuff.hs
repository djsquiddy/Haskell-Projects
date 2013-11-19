-- double x = x + x
-- quadruble x = double (double x)
-- quadruble x = ((\x -> x + x)(\x -> x + x) x)
factorial n = product [1..n]
average ns = sum ns `div` length ns

n = a `div` length xs
	where
		a = 25
		xs = [1,2,3,4,5]

fib n1 n2 = (n1+n2)(fib n2(n1+n2))
		
lastEle (x:[]) = x
lastEle (x:xs) = lastEle xs

rmLast (x:[]) acc = acc
rmLast (x:xs) acc = rmLast xs (acc ++ [x])


data Colors = RED|BLUE|YELLOW deriving (Show,Eq,Ord)
data Student = USU String Int|UofU String Int
data Address = None|Addr String --nullary|unary data constructors
data Tree a = Branch (Tree a) (Tree a)|Leaf a --recursive data type

homeless x =
	case x of
		None -> "homeless!"
		Addr y -> "You live at " ++ y
		
--size of a tree
sizeOf(Leaf a) = 1
sizeOf(Branch l r) = (sizeOf l) + (sizeOf r) + 1

length' [] = Nothing
length' (x:xs) = (length' xs) + 1 	-- : list construction operator

getSecond (x:xs) = head xs
getThird (x:xs) = head (tail xs)

addPair :: (Int,Int) -> Int
addPair (x,y) = x + y

fac 0 = 1
fac n = product[1..n]
fac2 n	|n < 0 = error"input to fac is negative"
		| n == 0 = 1
		| n > 0 = product[1..n]

dropEvery xs n = 
	if len > n then first ++ (dropEvery rest n)
	else xs
	where
		first = take (n-1) xs
		rest = drop n xs
		len = length xs
		
tail1 xs = if null xs then []
	else tail xs
tail2 :: [a] -> [a]
tail2 xs
	|null xs = xs	
	|otherwise = tail xs
tail3 [] = []
tail3 (x:xs) = xs

and1 a b = if a == b then True else False

nextLast [] = []
nextLast (_:xs) = xs !! (length xs) - 2

sameValues [] = True
sameValues (x:xs)
	| x == head xs = sameValues xs
	| otherwise = False

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys)

addPairs :: [(Integer, Integer)] -> [Integer]
addPairs xs = [x + y| (x,y) <- xs]

select xs = [x|x <- xs, x > 10]

bigger x y = [(a,b)| a <- x, b <- y, a < b]

removeAllItems xs num = [x| x <- xs, x /= num]
del e [] = []
del e (x:xs) = if e == x then del e xs else x:del e xs

removeListItems xs ys = [x| x <- xs, elem x ys == False]

mult xs ys = [x * y| x <- xs, y <- ys]

oddEle :: [Integer] -> [Bool]
oddEle [] = []
oddEle (x:xs) = if mod x 2 == 0 then False:(oddEle xs) else True:(oddEle xs)
oddEle' xs = [x `mod` 2 == 0| x <- xs]

factors num = [x| x <- [1..(num-1)], num `mod` x == 0 ]

remove y [] = []
remove y (x:xs) = if y== x then remove y xs else x:(remove y xs)
perms xs = [(x:y)| x <- xs, y <- perms (remove x xs)]

quickSort [] = []
quickSort (x:xs) = quickSort ([y| y <- xs, y < head xs]) ++ [x]:quickSort ([y|y <- xs, y >= x])

