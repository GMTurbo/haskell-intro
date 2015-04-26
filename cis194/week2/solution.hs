module LogAnalysis where

import Log


checkFirst :: String -> Bool
checkFirst letter = case letter of
  "I" -> True
  "W" -> True
  "E" -> True
  (_)  -> False

getLogType :: String -> MessageType
getLogType message =
  let split = words message
      first = head split
  in case first of
   "I" -> Info
   "W" -> Warning
   "E" -> Error (read (head (tail split)) :: Int)

-- parseRest :: String -> (Int, String)
-- parseRest str = let (t, rest) = lex str
--   in (read int :: Int, tail rest )

parseRest :: String -> (Int, String)
parseRest rest =
  let split = words rest
   in (read (head split) :: Int, unwords (tail split))

-- (Error 2) 562 "help help"
parseMessage :: String -> LogMessage
parseMessage message
  | checkFirst(head (words message)) =
       let logType = getLogType message
           shrunk = tail(words message)
           rest = parseRest(unwords shrunk)
           timestamp = fst rest
           last = snd rest
         in case logType of
           Error (x)-> let timestamp2 = (read (head (words last)) :: Int)
                           last2 = unwords(tail (words last))
                        in LogMessage logType timestamp2 last2
           (_) -> LogMessage logType timestamp last
  | otherwise = Unknown message

parse :: String -> [LogMessage]
parse str = map parseMessage2 entries
   where entries = (lines str) --filter (\s -> head (head s) =="E")

parseMessage2 :: String -> LogMessage
parseMessage2 str =
    let wordlist = words str in
     case wordlist of
      ("I":ts:msg) -> LogMessage Info (read ts :: Int) (unwords msg)
      ("W":ts:msg) -> LogMessage Warning (read ts :: Int) (unwords msg)
      ("E":lvl:ts:msg) -> LogMessage (Error (read lvl :: Int)) (read ts :: Int) (unwords msg)
      (_) -> Unknown (unwords wordlist)

-- Exercise 2

-- inserts a new LogMessage into an existing MessageTree, producing
-- a new MessageTree
-- insert :: LogMessage -> MessageTree -> MessageTree
-- insert log tree = case log of
--   Unknown -> tree
--   (_) -> let
