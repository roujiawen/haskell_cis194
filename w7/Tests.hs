-- Tests for Week 7 Exercise 2
-- (1) Add `import Tests` to the file that contains functions indexJ, dropJ,
--     and takeJ that you wrote (e.g. Solutions.hs)
-- (2) Make sure Solutions.hs doesn't have a redundant definition of JoinList
-- (3) Run the following to test your functions
--     ghci> :l Solutions.hs
--     ghci> testIndexJ indexJ
--     ghci> testDropJ dropJ
--     ghci> testTakeJ takeJ

module Tests where

import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

testCase1 = Empty

testCase2 = (Append (Size 1) Empty (Single (Size 1) "e"))

testCase3 = (Append (Size 1) (Single (Size 1) "e") Empty)

testCase4 = (Single (Size 1) "e")

testCase5 =
    (Append (Size 4)
        (Single (Size 1) "h")
        (Append (Size 3)
            (Single (Size 1) "y")
            (Append (Size 2)
              (Append (Size 1) (Single (Size 1) "e") Empty)
              (Single (Size 1) "a"))))

testCase6 =
    (Append (Size 4)
        (Single (Size 1) "h")
        (Append (Size 3)
            (Append (Size 2)
              (Single (Size 1) "a")
              (Append (Size 1) (Single (Size 1) "e") Empty))
            (Single (Size 1) "y")))

testCase7 =
    (Append (Size 4)
        (Append (Size 3)
            (Single (Size 1) "y")
            (Append (Size 2)
              (Append (Size 1) (Single (Size 1) "e") Empty)
              (Single (Size 1) "a")))
        (Single (Size 1) "h"))

testCases :: [JoinList Size [Char]]
testCases = [testCase1, testCase2, testCase3, testCase4, testCase5, testCase6, testCase7]

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _            = Nothing
_      !!? i | i < 0    = Nothing
(x:xs) !!? 0            = Just x
(x:xs) !!? i            = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

testIndexJ :: (Int -> JoinList Size [Char] -> Maybe [Char]) -> IO ()
testIndexJ indexJ
  | all (\(i, jl) -> (indexJ i jl) == (jlToList jl !!? i))
    [(i, jl) | i <- [-1..4], jl <- testCases] = putStr "Pass!\n"
  | otherwise = putStr $ "Failed!\nTest case: indexJ "++ show i ++ " (" ++ show jl
              ++ ")\nExpecting: " ++ show expecting
              ++ "\nGetting: " ++ show getting ++ "\n"
    where failedCase = head $ filter (\(i, jl) -> (indexJ i jl) /= (jlToList jl !!? i))
                        [(i, jl) | i <- [-1..4], jl <- testCases]
          expecting = (\(i, jl) -> (jlToList jl !!? i)) failedCase
          getting = (\(i, jl) -> (indexJ i jl)) failedCase
          i = fst failedCase
          jl = snd failedCase

testDropJ :: (Int -> JoinList Size [Char] -> JoinList Size [Char]) -> IO ()
testDropJ dropJ
  | all (\(i, jl) -> jlToList (dropJ i jl) == drop i (jlToList jl))
    [(i, jl) | i <- [-1..4], jl <- testCases] = putStr "Pass!\n"
  | otherwise = putStr $ "Failed!\nTest case: dropJ "++ show i ++ " (" ++ show jl
              ++ ")\nExpecting: " ++ show expecting
              ++ "\nGetting: " ++ show getting ++ "\n"
    where failedCase = head $ filter (\(i, jl) -> jlToList (dropJ i jl) /= drop i (jlToList jl))
                        [(i, jl) | i <- [-1..4], jl <- testCases]
          expecting = (\(i, jl) -> drop i (jlToList jl)) failedCase
          getting = (\(i, jl) -> jlToList (dropJ i jl)) failedCase
          i = fst failedCase
          jl = snd failedCase

testTakeJ :: (Int -> JoinList Size [Char] -> JoinList Size [Char]) -> IO ()
testTakeJ takeJ
  | all (\(i, jl) -> jlToList (takeJ i jl) == take i (jlToList jl))
    [(i, jl) | i <- [-1..4], jl <- testCases] = putStr "Pass!\n"
  | otherwise = putStr $ "Failed!\nTest case: takeJ "++ show i ++ " (" ++ show jl
              ++ ")\nExpecting: " ++ show expecting
              ++ "\nGetting: " ++ show getting ++ "\n"
    where failedCase = head $ filter (\(i, jl) -> jlToList (takeJ i jl) /= take i (jlToList jl))
                        [(i, jl) | i <- [-1..4], jl <- testCases]
          expecting = (\(i, jl) -> take i (jlToList jl)) failedCase
          getting = (\(i, jl) -> jlToList (takeJ i jl)) failedCase
          i = fst failedCase
          jl = snd failedCase
