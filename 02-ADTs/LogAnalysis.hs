{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

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
