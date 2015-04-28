module LogAnalysis where

import Log

-- Exercise 1
parse :: String -> [LogMessage]
parse str = map parseMessage entries
   where entries = (lines str) --filter (\s -> head (head s) =="E")

parseMessage :: String -> LogMessage
parseMessage str =
    let wordlist = words str in
     case wordlist of
      ("I":ts:msg) -> LogMessage Info (read ts :: Int) (unwords msg)
      ("W":ts:msg) -> LogMessage Warning (read ts :: Int) (unwords msg)
      ("E":lvl:ts:msg) -> LogMessage (Error (read lvl :: Int)) (read ts :: Int) (unwords msg)
      (_) -> Unknown (unwords wordlist)

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert logMsg1@(LogMessage _ tsNew _) (Node left logMsg2@(LogMessage _ tsOld _ ) right)
  | tsNew > tsOld = Node left logMsg1 (insert logMsg2 right)
  | otherwise = Node (insert logMsg1 left) logMsg2 right

insert logMsg Leaf =
  MessageTree Leaf logMsg Leaf
