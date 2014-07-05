{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Control.Applicative

-- | Parses an individual log line.
--
-- >>> parseMessage "E 2 562 help help"
-- LogMessage (Error 2) 562 "help help"
-- >>> parseMessage "I 29 la la la"
-- LogMessage Info 29 "la la la"
-- >>> parseMessage "This is not in the right format"
-- Unknown "This is not in the right format"
parseMessage :: String -> LogMessage
parseMessage m =
  case words m of
    ("E":code:timestamp:message) ->
      parseHelper (Error (read code)) timestamp message
    ("I":timestamp:message) ->
      parseHelper Info timestamp message
    ("W":timestamp:message) ->
      parseHelper Warning timestamp message
    _ -> Unknown m

{- | Parses a MessageType, string representing timestamp, and array of message
words into a LogMessage.

>>> parseHelper (Error 1) "3" ["Pickles","are","too","fuzzy"]
LogMessage (Error 1) 3 "Pickles are too fuzzy"

>>> parseHelper Info "8" ["Zoe","is","a","dog"]
LogMessage Info 8 "Zoe is a dog"

>>> parseHelper Warning "17" ["Nuclear","launch","detected"]
LogMessage Warning 17 "Nuclear launch detected"
-}
parseHelper :: MessageType -> String -> [String] -> LogMessage
parseHelper msgType timestamp message =
  LogMessage msgType (read timestamp) (unwords message)

-- | Parses multiple log lines.
--
-- >>> parse "E 2 562 help help\nI 29 la la la"
-- [LogMessage (Error 2) 562 "help help",LogMessage Info 29 "la la la"]
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- | Returns a new MessageTree with the given LogMessage inserted into the
-- correct sorted position.
--
-- >>> insert (Unknown "i'm an invalid message") Leaf
-- Leaf
-- >>> insert (LogMessage (Error 2) 562 "help help") Leaf
-- Node Leaf (LogMessage (Error 2) 562 "help help") Leaf
-- >>> insert (LogMessage Info 1 "") (Node Leaf (LogMessage Info 2 "") Leaf)
-- Node (Node Leaf (LogMessage Info 1 "") Leaf) (LogMessage Info 2 "") Leaf
-- >>> insert (LogMessage Info 3 "") (Node Leaf (LogMessage Info 2 "") Leaf)
-- Node Leaf (LogMessage Info 2 "") (Node Leaf (LogMessage Info 3 "") Leaf)
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ mStamp _) (Node l n@(LogMessage _ nStamp _) r) =
  if mStamp < nStamp
  then Node (insert m l) n r
  else Node l n (insert m r)
insert _ _ = Leaf

-- | Builds a MessageTree from the given list of LogMessages.
--
-- >>> build [(LogMessage Info 1 ""), (LogMessage Info 2 "")]
-- Node (Node Leaf (LogMessage Info 1 "") Leaf) (LogMessage Info 2 "") Leaf
-- >>> build []
-- Leaf
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- | Takes a sorted MessageTree and produces a list of all the LogMessages it
-- contains, sorted by timestamp from smallest to biggest.
--
-- >>> inOrder Leaf
-- []
-- >>> inOrder $ Node (Node Leaf (LogMessage Info 1 "") Leaf) (LogMessage Info 2 "") Leaf
-- [LogMessage Info 1 "",LogMessage Info 2 ""]
-- >>> inOrder $ Node Leaf (LogMessage Info 2 "") (Node Leaf (LogMessage Info 3 "") Leaf)
-- [LogMessage Info 2 "",LogMessage Info 3 ""]
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ m : inOrder r

-- | Returns a list of the messages corresponding to any errors with a severity
-- of 50 or greater, sorted by timestamp.
--
-- >>> whatWentWrong []
-- []
-- >>> whatWentWrong [LogMessage (Error 49) 1 "a", LogMessage (Error 50) 2 "b", LogMessage (Error 51) 3 "c"]
-- ["b","c"]
-- >>> whatWentWrong [LogMessage Info 3 "a", Unknown "hi"]
-- []
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong (LogMessage (Error sev) _ m : ms) =
  if sev >= 50
  then m : whatWentWrong ms
  else whatWentWrong ms
whatWentWrong (_:ms) = whatWentWrong ms

whoDidIt :: IO [String]
whoDidIt = map show . inOrder . build . parse <$> readFile "error.log"

printWhoDidIt :: IO ()
printWhoDidIt = do
  ls <- whoDidIt
  mapM_ putStrLn ls
