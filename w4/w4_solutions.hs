-- Exercise 1: Wholemeal programming
-- Reimplement each of the following functions in a more idiomatic
-- Haskell style. Use wholemeal programming practices, breaking each function
-- into a pipeline of incremental transformations to an entire data structure.
-- Name your functions fun1’ and fun2’ respectively.

-- Product of all the (even numbers - 2)
fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

-- Sum of even elements in hailstone sequence
fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (> 1) . iterate hailstone
  where hailstone n = if even n then n `div` 2 else 3 * n + 1




-- Exercise 2: Folding with trees
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

getHeight :: Tree a -> Integer
getHeight Leaf = -1
getHeight (Node height _ _ _) = height

addNode :: a -> Tree a -> Tree a
addNode node Leaf = Node 0 Leaf node Leaf
addNode node (Node _ leftTree value rightTree)
  | lh > rh   = let newRightTree = addNode node rightTree
    in Node (max lh (getHeight newRightTree) + 1) leftTree value newRightTree
  | otherwise = let newLeftTree = addNode node leftTree
    in Node (max (getHeight newLeftTree) rh + 1) newLeftTree value rightTree
  where lh = getHeight leftTree
        rh = getHeight rightTree

foldTree :: [a] -> Tree a
foldTree = foldr addNode Leaf




-- Exercise 3: More folds!
-- 1. Implement a function
--    xor :: [Bool] -> Bool
-- which returns True if and only if there are an odd number of True values
-- contained in the input list. It does not matter how many False values the
-- input list contains. For example,
-- xor [False, True, False] == True
-- xor [False, True, False, False, True] == False
-- Your solution must be implemented using a fold.

xor :: [Bool] -> Bool
xor = odd . foldr (\x y -> if x then y+1 else y) 0

-- 2. Implement map as a fold. That is, complete the definition
--   map' :: (a -> b) -> [a] -> [b]
--   map' f = foldr ...
-- in such a way that map’ behaves identically to the standard map function.

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a bs -> f a : bs) []

-- 3. (Optional) Implement foldl using foldr. That is, complete the definition
--    myFoldl :: (a -> b -> a) -> a -> [b] -> a
--    myFoldl f base xs = foldr ...
-- in such a way that myFoldl behaves identically to the standard foldl function.

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)




-- Exercise 4: Finding primes
-- Read about the Sieve of Sundaram. Implement the algorithm using function
-- composition. Given an integer n, your function should generate all the odd
-- prime numbers up to 2n + 2.
--  sieveSundaram :: Integer -> [Integer]
--  sieveSundaram = ...

-- Not sure how to solve this using only function composition...
