
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
