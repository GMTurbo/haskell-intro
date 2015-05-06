

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
