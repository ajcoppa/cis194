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
  if (< 3) . length . words $ m
  then Unknown m
  else
    let messageWords = words m
        typeCharacter = messageWords !! 0
    in case typeCharacter of "E" -> parseErrorMessage messageWords
                             "I" -> parseInfoMessage messageWords
                             "W" -> parseWarningMessage messageWords
                             _ -> Unknown m

-- | Parses an individual log line as an Error message.
-- 
-- >>> parseErrorMessage ["E","2","562","help","help"]
-- LogMessage (Error 2) 562 "help help"
parseErrorMessage :: [String] -> LogMessage
parseErrorMessage ws =
  let code = read $ ws !! 1
      timestamp = read $ ws !! 2
      message = unwords . drop 3 $ ws
  in LogMessage (Error code) timestamp message

-- | Parses an individual log line as an Info message.
-- 
-- >>> parseInfoMessage ["I","29","la","la","la"]
-- LogMessage Info 29 "la la la"
parseInfoMessage :: [String] -> LogMessage
parseInfoMessage ws =
  let timestamp = read $ ws !! 1
      message = unwords . drop 2 $ ws
  in LogMessage Info timestamp message

-- | Parses an individual log line as a Warning message.
-- 
-- >>> parseWarningMessage ["W","5","Flange","is","due","for","a","check-up"]
-- LogMessage Warning 5 "Flange is due for a check-up"
parseWarningMessage :: [String] -> LogMessage
parseWarningMessage ws =
  let timestamp = read $ ws !! 1
      message = unwords . drop 2 $ ws
  in LogMessage Warning timestamp message

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
