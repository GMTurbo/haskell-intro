

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


---- We begin to see a pattern

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
