{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

toDigit :: Char -> Int
toDigit '0' = 0
toDigit '1' = 1
toDigit '2' = 2
toDigit '3' = 3
toDigit '4' = 4
toDigit '5' = 5
toDigit '6' = 6
toDigit '7' = 7
toDigit '8' = 8
toDigit '9' = 9
toDigit _ = -1

-- isNum :: String -> Bool
-- isNum [] = False
-- isNum [x] = (toDigit x /= -1)
-- isNum (x:ys) = (toDigit x /= -1) && isNum ys


toDigitsRev :: String -> Int
toDigitsRev [x] = toDigit x
toDigitsRev (x:ys) = (toDigit x) + (toDigitsRev ys) * 10


toDigits :: String -> Int
toDigits x = toDigitsRev (reverse x)

split :: String -> [String]
split [] = [[], []]
split (' ':xs) = [[], xs]
split (x:ys) = [x:part1, part2] where [part1, part2] = split ys

-- splitN :: Int -> String -> String
-- splitN 1 string = string
-- splitN n string =


parseMessage :: String -> LogMessage
parseMessage 'I':' ':xs = parseInfo xs
parseMessage 'W':' ':xs = parseWarning xs
parseMessage 'E':' ':xs = parseError xs
parseMessage _ =
--
-- parseInfo :: String -> LogMessage
-- parseInfo string =
--   |firstPhrase string = Info TimeStamp String
--   |otherwise = Unknown "I " ++ string
--
-- parseWarning :: String -> LogMessage
--
-- parseError :: String -> LogMessage

--
--   LogMessage MessageType TimeStamp String
