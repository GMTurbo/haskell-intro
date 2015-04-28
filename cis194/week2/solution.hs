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
insert logMsg1@(LogMessage _ tsNew _) msgTree@(Node left logMsg2@(LogMessage _ tsOld _ ) right)
  | tsNew > tsOld = Node left logMsg1 (insert logMsg2 right)
  | otherwise = Node (insert logMsg1 left) logMsg2 right

insert logMsg Leaf =
  Node Leaf logMsg Leaf

--Exercise 3

build :: [LogMessage] -> MessageTree
build messages = case messages of
  []          -> Node Leaf (Unknown "empty") Leaf
  (lm1:lm2:_) -> insert lm2 (Node Leaf lm1 Leaf)

--Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder (Node left logMsg right) =
   (inOrder left) ++ [logMsg] ++ (inOrder right)
inOrder Leaf = []
--Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages =
   filter (filterFun 50) (inOrder (build messages)) --bug here

filterFun :: Int -> LogMessage -> Bool
filterFun min (LogMessage (Error msgSev) _ _)
 | msgSev > min = True
 | otherwise = False
