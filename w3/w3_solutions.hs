-- Exercise 1 Hopscotch
-- Your first task is to write a function
--  skips :: [a] -> [[a]]
-- The output of skips is a list of lists. The first list in the output should
-- be the same as the input list. The second list in the output should contain
-- every second element from the input list. . . and the nth list in the output
-- should contain every nth element from the input list.
-- For example:
--  skips "ABCD"       == ["ABCD", "BD", "C", "D"]
--  skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
--  skips [1]          == [[1]]
--  skips [True,False] == [[True,False], [False]]
--  skips []           == []
-- Note that the output should be the same length as the input.

skipN :: Int -> [a] -> [a]
skipN n lst = [snd pair | pair <- pairs, ((fst pair) `mod` n == 0)]
  where pairs = zip [1..] lst

skips :: [a] -> [[a]]
skips lst = [skipN n lst | n <- [1..(length lst)]]




-- Exercise 2 Local maxima
-- A local maximum of a list is an element of the list which is strictly greater
-- than both the elements immediately before and after it. For example, in the
-- list [2,3,4,1,5], the only local maximum is 4, since it is greater than the
-- elements immediately before and after it (3 and 1). 5 is not a local maximum
-- since there is no element that comes after it.
-- Write a function
--  localMaxima :: [Integer] -> [Integer]
-- which finds all the local maxima in the input list and returns them in order. For example:
--  localMaxima [2,9,5,6,1] == [9,6]
--  localMaxima [2,3,4,1,5] == [4]
--  localMaxima [1,2,3,4,5] == []

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [x] = []
localMaxima [x, y] = []
localMaxima (x:y:z:rest)
  | y > x && y > z = y : (localMaxima (y:z:rest))
  | otherwise = localMaxima (y:z:rest)




-- Exercise 3 Histogram
-- For this task, write a function
--  histogram :: [Integer] -> String
-- which takes as input a list of Integers between 0 and 9 (inclusive), and
-- outputs a vertical histogram showing how many of each number were in the
-- input list. You may assume that the input list does not contain any numbers
-- less than zero or greater than 9 (that is, it does not matter what your
-- function does if the input does contain such numbers). Your output must
-- exactly match the output shown in the examples below.

counter :: [Integer] -> [Int]
counter lst = [length (filter (== n) lst) | n <- [0..9]]

plotLine :: [Int] -> String
plotLine lst = map (\x -> (if maximum lst == x then '*' else ' ')) lst

plot :: [Int] -> String
plot lst
  | sum lst == 0 = "==========\n0123456789\n"
  | otherwise = plotLine lst ++ '\n' : plot (map reduceMax lst)
  where reduceMax = \x -> (if maximum lst == x then x-1 else x)

histogram :: [Integer] -> String
histogram lst = plot (counter lst)
