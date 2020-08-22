-- Exercise 1 We need to first find the digits of a number. Define the
-- functions
--  toDigits    :: Integer -> [Integer]
--  toDigitsRev :: Integer -> [Integer]
-- toDigits should convert positive Integers to a list of digits. (For 0 or
-- negative inputs, toDigits should return the empty list.) toDigitsRev should
-- do the same, but with the digits reversed.
-- Example: toDigits 1234 == [1,2,3,4]
-- Example: toDigitsRev 1234 == [4,3,2,1]
-- Example: toDigits 0 == []
-- Example: toDigits (-17) == []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | n < 10 = [n]
  | otherwise = (mod n 10) : toDigitsRev (div n 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)




-- Exercise 2 Once we have the digits in the proper order, we need to double
-- every other one. Define a function
--   doubleEveryOther :: [Integer] -> [Integer]
-- Remember that doubleEveryOther should double every other num- ber beginning
-- from the right, that is, the second-to-last, fourth-to-last, . . . numbers
-- are doubled.
-- Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- Example: doubleEveryOther [1,2,3] == [1,4,3]

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev ([x]) = [x]
doubleEveryOtherRev (x:y:zs) =  x : (y * 2): doubleEveryOtherRev zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lst = reverse (doubleEveryOtherRev (reverse lst))




-- Exercise 3 The output of doubleEveryOther has a mix of one-digit and
-- two-digit numbers. Define the function
--  sumDigits :: [Integer] -> Integer
-- to calculate the sum of all digits.
-- Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:ys) = (if x < 10 then x else div x 10 + mod x 10) + sumDigits ys




-- Exercise 4 Define the function
--  validate :: Integer -> Bool
-- that indicates whether an Integer could be a valid credit card number. This
-- will use all functions defined in the previous exercises.
-- Example: validate 4012888888881881 = True
-- Example: validate 4012888888881882 = False

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0




-- Exercise 5 The Towers of Hanoi.
-- To move n discs (stacked in increasing size) from peg a to peg b using
-- peg c as temporary storage,
-- 1. move n − 1 discs from a to c using b as temporary storage
-- 2. move the top disc from a to b
-- 3. move n − 1 discs from c to b using a as temporary storage.
-- For this exercise, define a function hanoi with the following type:
--  type Peg = String
--  type Move = (Peg, Peg)
--  hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a




-- Exercise 6 (Optional) What if there are four pegs instead of three?
-- Example: hanoi'NumMoves 15 "a" "b" "c" "d" = 129

-- Minimize the number of moves over variable m (1 <= m <= n - 1):
-- 1. move n − 1 discs from a to c & d
--    i. move m discs from a to c, using b & d as temporary storage
--    ii. move (n - 1) - m discs from a to d, using b as temporary storage
-- 2. move the top disc from a to b
-- 3. move n − 1 discs from c & d to b
--    i. move (n - 1) - m discs from d to b, using a as temporary storage
--    ii. move m discs from c to b, using a & d as temporary storage

hanoiNumMoves :: Integer -> Peg -> Peg -> Peg -> Integer
hanoiNumMoves n a b c = toInteger (length (hanoi n a b c))

listOfPossibleM's :: Integer -> [(Integer, Integer)]
listOfPossibleM's n = zip [1..(n-1)] (reverse [1..(n-1)])

hanoi'CandidateSolution :: Peg -> Peg -> Peg -> Peg -> (Integer, Integer) -> Integer
hanoi'CandidateSolution a b c d (m, rest) = hanoi'NumMoves m a c b d
                                          + hanoiNumMoves rest a d b
                                          + 1
                                          + hanoiNumMoves rest d b a
                                          + hanoi'NumMoves m c b a d

hanoi'NumMoves :: Integer -> Peg -> Peg -> Peg -> Peg -> Integer
hanoi'NumMoves n a b c d
 | n <= 2 = hanoiNumMoves n a b c
 | otherwise = minimum (map (hanoi'CandidateSolution a b c d) (listOfPossibleM's (n-1)))
