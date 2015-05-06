

-- Familiar examples of monads
-- Common functionality for all Monads
-- Monad type class
-- do-notation

-------- Monad Examples --------
-------- IO --------

--returns allows you to make any input an IO
return :: a -> IO a
-- once something is IO boxed, it cannot be unboxed
-- so
unreturn :: IO a -> a --THIS IS IMPOSSIBLE

--temporarily unpacks value, modifies value, and returns new IO
bindIO :: IO a -> (a -> IO b) -> IO b

-- You can put a value into IO and manipulate them as long as you leave them in IO

-------- List --------
singleton :: a -> [a]
unsingleton :: [a] -> a -- we cannot unpack a list of values into a value

-- given a list and a function, we return a new list
flatMap :: [a] -> (a -> [b]) -> [b]

flatMap [1,7,11] (\x -> [x, x+1])
--result [1,2,7,8,11,12]

-------- Maybe --------
data Maybe a = Nothing | Just a

Just :: a -> Maybe a

unJust :: Maybe a -> a -- DOESN'T EXIST

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b

bindMaybe Nothing (\x ->
  if (x == 0)
  then Nothing
  else Just (2*x))
-- Result: Nothing

bindMaybe (Just 0) (\x ->
  if (x == 0)
  then Nothing
  else Just (2*x))
-- Result: Nothing

bindMaybe (Just 1) (\x ->
  if (x == 0)
  then Nothing
  else Just (2*x))
-- Result: Just 2


-------- Common Functionality for All Monads --------

-- the following 3 function take a value and package it
return    :: a -> IO a
singleton :: a -> [a]
just      :: a -> Maybe a

-- the following 3 function allow us to modify packaged values
-- provided you package the result up before you finish
bindIO    :: IO a    -> (a -> IO b)    -> IO b
flatMap   :: [a]     -> (a -> [b])     -> [b]
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b

-- So, in order to be a Monad,
---- it has to have a way to package a value
---- and a way to modify and repackage

-- Any Monad must support a return function and a bind function
-- all modifying functions are so similiar we can just use 1 name
bind :: IO a    -> (a -> IO b)    -> IO b
bind :: [a]     -> (a -> [b])     -> [b]
bind :: Maybe a -> (a -> Maybe b) -> Maybe b

-- Join removes outer layer
join :: IO (IO a) -> IO a
join :: [[a]] -> [a]
join :: Maybe (Maybe a) -> Maybe a
join mmx = bind mmx id

join [[1,2,3], [4,5,6]]
--Result: [1,2,3,4,5,6]

join (Just (Just 7))
--Result: Just 7

join (Just Nothing)
--Result: Nothing

join Nothing
--Result: Nothing

-------- Monad Type Class --------

class Monad m where -- m is the monad type
  return :: a -> m a -- must have a return function
  (>>=)  :: m a -> (a -> m b) -> m b -- the bind operation

-- above captures common pattern in IO, list, and Maybe

join :: Monad m => m (m a) -> m a
join mmx = mmx >>= id

-- Type class of parameterized types
-- Monad laws (if you're going to write your own Monad
---- there are more conditions that must be satisfied)


-------- do-Notation --------
addM :: Monad m => m Int -> m Int -> m Int
addM mx my =
  mx >>= (\x -> my >>= (\y -> return (x + y)))

addM' :: Monad m => m Int -> m Int -> m Int
addM' mx my = do
  x <- mx
  y <- my
  return (x + y)

--do
--  x <- mx -- binds value of mx into x
--  ...
-- turns into
--mx >>= (\x -> ... )


people = ["alice", "bob", "eve"]
items = ["car", "puppy"]
missing = do
  person <- people
  item <- items
  return (person ++ " lost a " ++ item)

missing
--Result:
--["alice lost a car"
--,"alice lost a puppy"
--,"bob lost a car"
--,"bob lost a puppy"
--,"eve lost a car"
--,"eve lost a puppy"
--]


-------- Common Monads --------

-- Reader
-- State
-- ST

---- Reader Monad ---
-- used to represent computations that need access to some context value
-- like the name of the current user

import Control.Monad.Reader

data Reader r a
-- r = type of context value which can be read within the reader monad
-- a = result of reader computation

instance Monad (Reader r)

ask :: Reader r r

runReader :: Reader r a -> r -> a

getFirst :: Reader String String
getFirst = do
  name <- ask -- get name from Reader Monad
  return (name ++ " woke up") -- repackaged into Reader Monad
getSecond :: Reader String String
getSecond = do
  name <- ask -- get name from Reader Monad
  return (name ++ " wrote some haskell") -- repackaged into Reader Monad
getStory :: Reader String String
getStory = do
  first <- getFirst
  second <- getSecond
  return ("First, " ++ first ++
          ".  Second, " ++ second ++ ".")
story = runReader getStory "Gabe"

story
--Result: "First, Gabe woke up.  Second, Gabe wrote some haskell"

---- State Monad ---
-- used to write code that requires state that could change during computation

import Control.Monad.State

data State s a
-- s = type of state
-- a = type of result

get :: State s s
put :: s -> State s () -- () result type is Unit
evalState :: State s a -> s -> a

harmonicStep :: State (Double, Double) Double
harmonicStep = do
  (position, velocity) <- get --get current state
  let acceleration = (-0.01 * position)
      velocity'    = velocity + acceleration
      position'    = position + velocity'
  put (position', velocity') -- put new position and velocity back
  return position

harmonic :: State (Double, Double) [Double]
harmonic = do
  position <- harmonicStep
  laterPositions <- harmonic -- infinite list, but no problem in haskell
  return (position : laterPositions)

let position = evalState harmonic (1,0)
take 4 positions
--Result:
--[1.0,
-- 0.99,
-- 0.9701,
-- 0.940499]

newtype State s a = State (s -> (a, s))

---- ST Monad ---
-- fancy State Monad
-- Implement imperative algorithms
-- Modifiable values
-- Pure from the outside

import Control.Monad.ST

data ST s a
-- a = result type of computation
-- s = ignore for ST Monad

instance Monad (ST s)

runST :: ST s a -> a -- returns boxed value

-- you need a special value to use with ST
import Data.STRef

data STRef s a

newSTRef :: a -> ST s (STRef s a)
readSTRef :: STRef s a -> ST s a
writeSTRef :: STRef s a -> a -> ST s ()

sumST :: [Int] -> STRef s Int -> ST s ()
sumST []     accumRef = return ()
sumST (x:xs) accumRef = do
  accum <- readSTRef accumRef
  writeSTRef accumRef (x + accum)
  sumST xs accumRef

sum' :: [Int] -> Int
sum' xs = runST $ do
  accumRef <- newSTRef 0
  sumST xs accumRef
  readSTRef accumRef

sum' [1,1,1,1,1,1,1]
--RESULT: 6

---- ST MONAD USES
-- High Performance
-- Translating imperative code
-- Complicated, multi-part state

import Control.Monad

-------- Monadic Flow Control --------
-- Analogous with imperative flow control constructs
-- Actually just ordinary functions

---- forM ----
-- equivalent to ForEach --
forM :: Monad m => [a] -> (a -> m b) -> m [b]

--forM version if you want to disregard the result
forM_ :: Monad m => [a] -> (a -> m b) -> m ()

forM list $ do
  x <- first_action
  second_action
  ...

forM [1,2,3] print
--1
--2
--3

replicateM  :: Monad m => Int -> m a -> m [a]
replicateM_ :: Monad m => Int -> m a -> m ()

replicateM 3 (putStrLn "hello")
--hello
--hello
--hello

when :: Monad m => Bool -> m () -> m ()

when debug (putStrLn "Debugging") -- prints Debugging if debug is true

---- Lifting: liftM ----

liftM :: Monad m => (a -> b) -> (m a -> m b)

liftM (1+) (Just 3)
--RESULT: Just 4

liftM2 :: Monad m =>
            (a1->a2->b) -> m a1 -> m a2 -> m b
liftM3 :: ...
liftM4 :: ...
liftM5 :: ...

liftM2 (+) (Just 3) (Just 5)
--RESULT: Just 8

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
forM :: Monad m => [a] -> (a -> m b) -> m [b] -- args reversed

mapM print [1,2,3]
--1
--2
--3

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]

askToKeep :: Int -> IO Bool
askToKeep x = do
  putStrLn ("keep " ++ (show x) ++ "?")
  (c : _) <- getLine
  return (c == 'y')

askWhichToKeep :: [Int] -> IO [Int]
askWhichToKeep xs =
  filterM askToKeep xs


foldM :: Monad m => (a -> b -> m a) ->
                     a -> [b] -> m a

sayAddition :: Int -> Int -> IO Int
sayAddition x y = do
  let z = x + y
  putStrLn ((show x) ++ " + " ++
            (show y) ++ " = " ++
            (show z))
  return z

talkingSum :: [Int] -> IO Int
talkingSum xs = foldM sayAddition 0 xs
