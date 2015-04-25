head' :: [a] -> a

head' nums = case nums of
   []     -> error "empty list"
   (x:xs) -> x

length' :: [a] -> Int

length' nums = case nums of
   []    -> 0
   (x:xs)-> length xs + 1

-- badSum :: [a] -> a -- this says any type can be summed
--  -- which isn't true
-- badSum nums = case nums of
--     []    -> 0
--     (x:xs)-> x + (sum xs)

-- this sums any array of type num
--   and returns the concrete type
-- => indicates a constraint
goodSum :: Num a => [a] -> a

goodSum nums = case nums of
    []    -> 0
    (x:xs)-> x + (sum xs)
