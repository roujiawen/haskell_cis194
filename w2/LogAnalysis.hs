{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Text.Read

-- Exercise 1 The first step is figuring out how to parse an individual message.
-- Define a function
--  parseMessage :: String -> LogMessage
-- which parses an individual line from the log file. For example,
--  parseMessage "E 2 562 help help"
--      == LogMessage (Error 2) 562 "help help"
--  parseMessage "I 29 la la la"
--      == LogMessage Info 29 "la la la"
--  parseMessage "This is not in the right format"
--      == Unknown "This is not in the right format"

parseError :: [String] -> LogMessage
parseError (x:y:rest) =
  case errCodeMaybe of
    Just errCode ->
      case timeStampMaybe of
        Just timeStamp -> LogMessage (Error errCode) timeStamp (unwords rest)
        Nothing        -> Unknown ("E " ++ (unwords (x:y:rest)))
    Nothing      -> Unknown ("E " ++ (unwords (x:y:rest)))
  where errCodeMaybe   = readMaybe x :: Maybe Int
        timeStampMaybe = readMaybe y :: Maybe Int
parseError wordList = Unknown ("E " ++ (unwords wordList))

parseInfo :: [String] -> LogMessage
parseInfo (x:rest) =
  case timeStampMaybe of
    Just timeStamp -> LogMessage Info timeStamp (unwords rest)
    Nothing        -> Unknown ("I " ++ (unwords (x:rest)))
  where timeStampMaybe = readMaybe x :: Maybe Int
parseInfo wordList = Unknown ("I " ++ (unwords wordList))

parseWarning :: [String] -> LogMessage
parseWarning (x:rest) =
  case timeStampMaybe of
    Just timeStamp -> LogMessage Warning timeStamp (unwords rest)
    Nothing        -> Unknown ("W " ++ (unwords (x:rest)))
  where timeStampMaybe = readMaybe x :: Maybe Int
parseWarning wordList = Unknown ("W " ++ (unwords wordList))

parseWords :: [String] -> LogMessage
parseWords ("E":rest) = parseError rest
parseWords ("I":rest) = parseInfo rest
parseWords ("W":rest) = parseWarning rest
parseWords wordList = Unknown (unwords wordList)

parseMessage :: String -> LogMessage
parseMessage string = parseWords (words string)

-- Once we can parse one log message, we can parse a whole log file. Define a
-- function
--  parse :: String -> [LogMessage]
-- which parses an entire log file at once and returns its contents as a list of
-- LogMessages.
-- Test function:
--  testParse parse 10 "error.log"

parse :: String -> [LogMessage]
parse string = map parseMessage (lines string)




-- Exercise 2 Define a function
--  insert :: LogMessage -> MessageTree -> MessageTree
-- which inserts a new LogMessage into an existing MessageTree, pro- ducing a
-- new MessageTree. insert may assume that it is given a sorted MessageTree, and
-- must produce a new sorted MessageTree containing the new LogMessage in
-- addition to the contents of the original MessageTree.
-- However, note that if insert is given a LogMessage which is Unknown, it
-- should return the MessageTree unchanged.

getTimeStamp :: LogMessage -> Int
getTimeStamp (LogMessage _ timeStamp _) = timeStamp
getTimeStamp (Unknown _) = -1

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert newlog Leaf = Node Leaf newlog Leaf
insert newlog (Node leftTree parentLog rightTree)
  | (getTimeStamp newlog) < (getTimeStamp parentLog) = Node (insert newlog leftTree) parentLog rightTree
  | (getTimeStamp newlog) > (getTimeStamp parentLog) = Node leftTree parentLog (insert newlog rightTree)




-- Exercise 3 Once we can insert a single LogMessage into a MessageTree, we can
-- build a complete MessageTree from a list of messages. Specifi- cally, define
-- a function
--  build :: [LogMessage] -> MessageTree
-- which builds up a MessageTree containing the messages in the list, by
-- successively inserting the messages into a MessageTree (beginning with a
-- Leaf).

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (newlog:rest) = insert newlog (build rest)




-- Exercise 4 Finally, define the function
--  inOrder :: MessageTree -> [LogMessage]
-- which takes a sorted MessageTree and produces a list of all the LogMessages
-- it contains, sorted by timestamp from smallest to biggest. (This is known as
-- an in-order traversal of the MessageTree.)
-- With these functions, we can now remove Unknown messages and sort the
-- well-formed messages using an expression such as:
--  inOrder (build tree)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree parentLog rightTree) = (inOrder leftTree) ++ [parentLog] ++ (inOrder rightTree)




-- Exercise 5 Now that we can sort the log messages, the only thing left to do
-- is extract the relevant information. We have decided that “relevant” means
-- “errors with a severity of at least 50”.
-- Write a function
--  whatWentWrong :: [LogMessage] -> [String]
-- which takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater, sorted
-- by timestamp. (Of course, you can use your functions from the previous
-- exercises to do the sorting.)

getErrorCode :: LogMessage -> Int
getErrorCode (LogMessage (Error errCode) _ _) = errCode
getErrorCode _ = -1

extractSevereErrors :: [LogMessage] -> [LogMessage]
extractSevereErrors [] = []
extractSevereErrors (logMessage:rest)
  | (getErrorCode logMessage) >= 50 = logMessage:rest
  | otherwise = extractSevereErrors rest

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ message) = message
getMessage _ = ""

isError :: LogMessage -> Bool
isError (LogMessage (Error _) _ _) = True
isError _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMessages = map getMessage (extractSevereErrors sortedErrors)
  where sortedErrors = inOrder (build (filter isError logMessages))
