-- Name		: Dylan Jones
-- Project	: Haskell Functions 2(Homework 5)
-- Date		: 3/22/2012

-- Problem 1
-- Function Call: maxSub [1..5]
maxSub :: [Integer] -> Integer
maxSub xs = maxSubCalculation xs 0
	where
		maxSubCalculation [] currentSum = currentSum
		maxSubCalculation (x:xs) currentSum = if currentSum > greatest then currentSum else greatest
			where
				first = maxSubCalculation xs 0
				second = maxSubCalculation xs (currentSum + x)
				greatest = if first > second then first else second
		
-- Problem 2
-- Function Call: subst "this is the original string" "original" "replacement"
subst :: [Char] -> [Char] -> [Char] -> [Char]
subst xs target replacement = subStr xs target replacement [] [] 0
	where
		subStr [] _ _ prevList _ _ = prevList
		subStr (x:xs) target replacement prevList matchingList currLength
			| x == n && ((currLength + 1) == len) = (prevList ++ replacement ++ xs)
			| x == n = subStr xs target replacement prevList (matchingList ++ [x]) (currLength + 1)
			| x /= n = subStr xs target replacement (prevList ++ matchingList ++ [x]) [] 0
			where
				len = length target
				n = target !! currLength
 
-- Problem 3
data Tree = Empty | Node Integer Tree Tree deriving(Eq,Show)

-- Part A
-- Function Call: insertNode 5 Empty
-- Function Call: insertNode 5 (makeTree [1..4])
insertNode :: Integer -> Tree -> Tree
insertNode x Empty = (Node x Empty Empty)
insertNode x (Node root left right)
	| root < x = (Node root left (insertNode x right))
	| root > x = (Node root (insertNode x left) right)
	| otherwise = (Node root left right)

-- Part B
-- Function Call: makeTree [1..5]
makeTree :: [Integer] -> Tree
makeTree [] = Empty
makeTree xs = foldr (insertNode) Empty (reverse xs)

-- Part C
-- Function Call: createBal [1..5]
createBal :: [Integer] -> Tree
createBal [] = Empty		
createBal xs = makeTree (createBalList xs)
	where
		createBalList [] = []
		createBalList [x] = [x]
		createBalList xs = r: ((createBalList right) ++ (createBalList left))
			where
				(left, (r:right)) = splitAt ((length xs) `div` 2) xs

-- Part D
-- Function Call: collectNodes (createBal [1..5]) 2
collectNodes :: Tree -> Integer -> [Integer]
collectNodes Empty _ = []
collectNodes (Node root left right) 0 = [root] 
collectNodes (Node root left right) index = collectNodes left (index - 1) ++ collectNodes right (index - 1)

-- Part E
-- Function Call: deleteNode (createBal [1..5]) 3
deleteNode :: Tree -> Integer -> Tree
deleteNode Empty _ = Empty
deleteNode (Node root left right) x
	| left == Empty && right == Empty = Empty
	| root < x = (Node root left (deleteNode right x))
	| root > x = (Node root (deleteNode left x) right)
	| root == x = (Node n left (deleteNode right n))
	where
		leftMost Empty = 0
		leftMost (Node root left right) = if left == Empty then root else leftMost left
		n = leftMost right

-- Problem 4
-- Function Call: perms [1..3]
perms :: (Eq a) => [a] -> [[a]]
perms [] = [[]]
perms xs = [x:y| x <- xs, y <- perms (remove x xs)]
	where
		remove _ [] = []
		remove y (x:xs) = if y == x then xs else x:(remove y xs)
		
-- Problem 5
-- Function Call: symmetric [1..3]
symmetric :: [Integer] -> [[Integer]]
symmetric [] = [[]]
symmetric xs = [x| x <- (perms xs), (isTreeSymmetric (createBal x))]
	where
		isSymmetric Empty Empty = True
		isSymmetric left Empty = False
		isSymmetric Empty right = False
		isSymmetric (Node _ l1 r1) (Node _ l2 r2) = isSymmetric l1 r2 && isSymmetric r1 l2
		isTreeSymmetric Empty = True
		isTreeSymmetric (Node _ left right) = isSymmetric left right