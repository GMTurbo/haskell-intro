
---------- Overview ----------
-- Compile executable program
-- IO Actions
-- Combine IO Actions
-- IO Values
-- Useful IO Functions

---------- Hello World ----------
main = putStrLn "Hello World"

-- save as .hs
-- in terminal 'ghc HelloWorld.hs'
-- now you have a .exe!

---- HOW DOES THIS WORK?! -----
-- Haskell functions are pure
-- -> cannot modify any external state
-- -> Value cannot depend on external state
-- -> Cannot write to the console

---------- IO Actions ----------
:t putStrLn --Result: String -> IO ()

putStrLn :: String -> IO ()
--take a string and returns something
-- just defines code that COULD BE RUN

-- () -> data type of Unit
data Unit = Unit -- placeholder that contains no data

-- IO
data IO a

--so  putStrLn returns an IO Unit
-- and an IO Unit is an IO Action

-- putStrLn doesn't do anything unless it is uses
-- the IO action in main
main :: IO ()

-- since main is returned from putStrLn
-- main is SPECIAL
-- -> main - IO action executed by the program (side affects and all)

main2 = putStrLn "Hello World 2" -- this will not print

---------- do-Blocks ----------
main :: IO()
main = do
  putStrLn "Hello"
  putStrLn "World"

--Hello
--World

helloWorld :: IO ()
helloWorld = putStrLn "Hello World"

main :: IO()
main = do
  helloWorld
  helloWorld
  helloWorld

--Hello World
--Hello World
--Hello World

introduce :: String -> String -> IO ()
introduce name1 name2 = do
  putStrLn (name1 ++ ", this is " ++ name2)
  putStrLn (name2 ++ ", this is " ++ name1)

main :: IO ()
main = do
  introduce "Alice" "Bob"
  introduce "Alice" "Sally"

---------- IO Values ----------

main :: IO ()
main = do
  line <- getLine
  putStrLn ("You said: " ++ line)

--input: blah blah blah
--output: You said: blah blah blah

:t getLine
-- result: getLine :: IO String

greet::IO ()
greet = do
  putStrLn "Who are you?"
  who <- getLine
  putStrLn ("Hello " ++ who)

greetForever :: IO ()
greetForever = do
  greet
  greetForever

main :: IO ()
main = greetForever

---------- return Function ----------
-- useful for combining multiple IO values

dummyGetLine :: IO String
dummyGetLine =
  return "I'm not really doing anything"

main :: IO ()
main = do
  line <- dummyGetLine
  putStrLn line

return :: a -> IO a

--returning multiple IO values

promptInfo :: IO (String, String)
promptInfo = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn "What is your favorite color?"
  color <- getLine
  return (name, color) -- needs to return an IO Action

main :: IO ()
main = do
  (name, color) <- promptInfo -- pattern matching
  putStrLn ("Hello " ++ name)
  putStrLn ("I like " ++ color ++ " too!")

-- combining

main :: IO ()
main = do
  line1 <- getLine
  line2 <- getLine
  let lines = line1 ++ line2
  putStrLn lines

-- return doesn't stop program from running
-- return doesn't do anything to control flow
main :: IO ()
main = do
  return 0 -- just a function that creates an IO Action
  putStrLn "haha, still running"
  return "halt!"
  putStrLn "you can't stop me!"


-------- Some Useful IO Actions --------

putStrLn :: String -> IO ()
getLine :: IO String
print :: (Show a) => a -> IO () -- type must have a string rep (Show Type Class)
readFile :: FilePath -> IO String
-- Read an entire file as a (lazy) string
--  so you only ever evaluate parts you use
writeFile :: FilePath -> String -> IO ()
-- FilePath is just a type synonym for String
appendFile :: FilePath -> String -> IO ()

interact :: (String -> String) -> IO ()
-- takes an arg that maps a string to a string

reverseLines :: String -> String
reverseLines input =
  unlines (map reverse (lines input))

main :: IO ()
main = interact reverseLines

-------- Program Organization --------
-- Do as little IO as possible

--simple shift encryption
encrypt :: Char -> Char
encrypt c
 | 'A' <= c && c <'Z' | 'a' <= c && c <'z' =
      toEnum (fromEnum c + 1) -- a->b, c->d etc
 | c == 'Z' | c == 'z'= 'A'  -- wrap z
 | otherwise = c

-- decrypt
decrypt :: Char -> Char
decrypt c
  | 'A' <= c && c <'Z' | 'a' <= c && c <'z' =
       toEnum (fromEnum c - 1) -- a->b, c->d etc
  | c == 'A' | c == 'a'= 'Z' -- wrap z
  | otherwise = c

main :: IO ()
main = interact (map (encrypt . decrypt . encrypt))
