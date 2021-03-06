
--HASKELL AND TAB INDENTATION IS SUPER DANGEROUS
-- DO NOT USE TABS WITH HASKELL UNLESS YOUR IDE
--  HAS AN OPTION TO SWAP TAB TO SPACES

string1 = "hello"
string2 = "world"
greeting = string1 ++ " " ++ string2
arr = [1,2,3,4,5,6,7,8,9,10]
-- removeOdd nums =
--   if null nums
--   then []
--   else
--     if (mod (head nums) 2) == 0 -- even?
--     then (head nums) : (removeOdd (tail nums)) -- : is concatinate
--     else removeOdd (tail nums)

removeEvens nums =
  if null nums
  then []
  else
    if (mod (head nums) 2) == 1
    then (head nums) : (removeEvens (tail nums))
    else removeEvens(tail nums)

headAndLength list = (head list, length list)

-- pattern matching
-- double [] = []
-- double (x : xs) = -- double [1,2,3] => double (1, [2,3])
--   (2*x) : (double xs)

-- guard
pow2 n
  | n == 0  = 1
  | otherwise = 2 * (pow2 (n-1))

-- pattern matching and guard
-- removeOdd [] = []
-- removeOdd (x : xs)
--   | mod x 2 == 0  = x : (removeOdd xs)
--   | otherwise     = removeOdd xs

removeOdd nums = case nums of
  []      -> []
  (x:xs)  -> if mod x 2 == 0 then x : (removeOdd xs) else removeOdd xs

--case expression
double nums = case nums of
  []      -> []
  (x: xs) -> (2 * x) : (double xs)

anyEven nums = case (removeOdd nums) of
  []      -> False
  (x:xs)  -> True


-- let binding
-- bottom up
fancySeven =
  let a = 2
  in 2 * a + 1

fancyNine =
  let x = 5
      y = 4
  in x + y

numEven nums =
  let evenNums = removeOdd nums
  in length evenNums

-- where binding (must be associated with a function definition)
-- top down
fancySeven2 = 2 * a + 1
  where a = 3

fancyNine2 = x + y
  where x = 4
        y = 5

--fancyTen = 2 * (a + 1 where a = 4) NOT ALLOWED
--fancyTen = 2 * (let a = 4 in a + 1) works though

fancyTen = 2 * (a + 1)
  where a = 4

  -------------- Lazy Function Evaluation

-- Lazy Infinite Lists
intsFrom n = n : (intsFrom (n+1))
ints = intsFrom 1

take 100 ints


--        Higher-Order functions
pass3 f = f 3 -- takes function 3 and passes it 3
add1 x = x + 1
pass3 add1 -- 4

compose f g x = f (g x)
mult2 x = 2 * x
compose add1 mult2 4 -- (2 * 4) + 1 = 9

(const 7) 5 -- given any input will always return 7 (const 7) returns a function

-- partial application

foo x y z = x + y + z

foo_1_2 = foo 1 2 -- a partially applied foo function

pass x f = f x
(+) 4 5 -- 9 and is equivalent to 4 + 5

pass_3_4 f = f 3 4
pass_3_4 (+) -- 7
pass_3_4 (-) -- -1
pass_3_4 (*) -- 12

-- operator definition
(a,b) .+ (c,d) = (a + c, b + d)

plus1 = (+) 1
plus1 2 -- 3

plus1' = (1+)
plus1'' = (+1)

-- turning functions into operator
 mod 10 2 -- old
 10 `mod` 2 -- new

 -- MAP
map length ["hello", "abc", "1234"] -- [5,3,4]
map (1+) [1,3,5,7] -- [2,4,5,8]

double = map (2*)

-- FILTER
notNull xs = not (null xs)
filter notNull ["", "abc", "", "hello", ""]

isEven x = x `mod` 2 == 0
removeOdd = filter isEven

map snd (filter fst[(True, 1), (False, 7), (True,11)])

-- FOLD
foldl (+) 0 [1,2,3,4] -- reduce / aggregate SLIGHTLY FASTER
foldr (+) 0 [1,2,3,4] -- WORKS WITH INFINITE LISTS

-- ZIP Combining lists element by element
zip [1,2,3] [4,5,6] -- size is of result is min(l1,l2)
[(1,4), (2,5), (3,6)]

-- ZIPWITH
zipWith (+) [1,2,3] [4,5,6]
--[5,7,9]

plus3 x y z = x + y + z
zipWith3 plus3 [1,2,3] [4,5,6] [4,5,6]

zipWith4 (+) [1,2,3] [4,5,6] [4,5,6] [4,5,6]

-- LAMBDA Functions

-- WAS: plus3 x y z = x + y + z
-- LAMBDA: (\x y z -> x + y + z)
zipWith3 (\x y z -> x + y + z) [1,1,1] [1,1,1] [1,1,1]
--[3,3,3]

map (\x->2*x) [1,2,3]
--[2,4,6]


--- FUNCTION OPERATORS
--(.) - Function Composition
--($) - Function Application
stringLength = length . show -- first applies show, then length
stringLength' x = length (show x)  -- equivalent

notNull = not . null -- first check for null then negate it

-- to compose two functions, they both must have only 1 argument
f a b = a + b
g x = 2 * x

f . g -- DOESN'T WORK
g . f -- DOESN'T WORK

-- $ helps to avoid deeply nested parentheses
f $ x = f x
f $ g x = f (g x)
f $ g $ h $ k x = f (g (h (k x)))

--$ useful in higher order functions
map (\f -> f 3) [(+1), (\x-> 2*x + 3), (*2)] -- [4,9,6]
-- with a partially applied function using $
map ($3) [(+1), (\x-> 2*x + 3), (*2)] -- [4,9,6]

zipWith ($) [(+1), (\x-> 2*x + 3), (*2)] [1,2,3] --[2,7,6]

---------------------- TYPES ------------------

function x(arr){
  if(arr.length > 0)
  switch(arr[0]){

  }
}
foo x = x + 1
:t foo -- foo :: Num a => a -> a

foo x y z = x + y + z
:t foo -- foo :: Num a => a -> a -> a -> a

let x = 3 :: Int

-- it can be dangerous to type things yourself sometime
-- you can cause errors
-- for example
y = (3 :: Int) + (2.1 :: Double) -- can't add int to double

------- Type Inference ------

square x = x * x -- compiler infers x is a number

squareTwice x = square (square x) -- infers return type of square as number

-- can't concate a number to a string
brokenShowSquare x = "this is the value of square x: " ++ square x

workingShowSquare x = "this is the value of square x: " ++ show (square x )

-- Explicit typing is used more for readability
-- and communicating with people

-- YOU MOST LIKELY WILL NEVER NEED TO EXPLICITLY TYPE THINGS

-- tells us the function takes a string and returns an Int
mystery :: [Char] -> Int

-- this will error out
whats_wrong = x + y
    where x = length "hello"
          y = 6/2

-- THIS DOESNT WORK
str = show (read "123") -- read converts but requires a type

--THIS WORKS
str = show (read "123" :: Int)


------- Polymorphic Functions ------
-- NOT OBJECT ORIENTED POLYMORPHISM --
-- MORE SIMILIAR TO C# GENERICS OR TEMPLATES--

:t length
length = [a] -> Int --[a] implied it can handle different types of arrays
--length is polymorphic

length' [] = []
length' (x:xs) = length xs + 1

--type variables - start lowercase
-- a, b, x, foo, hello_123

--Concrete types - start Uppercase
-- Int, Integer, Char, Double

:t head -- polymorphic
head :: [a] -> a

head' nums = case nums of
   []     -> error "empty list"
   (x:xs) -> x

--compile rejects this
badHead :: [a] -> b
badHead (x:xs) -> x

---- Type Class Constaints ----

badSum :: [a] -> a -- this says any type can be summed
 -- which isn't true
badSum nums = case nums of
    []    -> 0
    (x:xs)-> x + (sum xs)

-- this sums any array of type num
--   and returns the concrete type
-- => indicates a constraint (type Numeric)
goodSum :: Num a => [a] -> a

goodSum nums = case nums of
    []    -> 0
    (x:xs)-> x + (sum xs)

-- you can have multiple constraints
showSum :: (Num a, Show a) => [a] -> [Char]
-- Show Type Class just means show can
--  be called on it (it has a string representation)

showSum x = show (sum x)

-- :t 3 -- Num a => a
-- :t 3.1 -- Fractional a => a

parseRest :: String -> (Int, String)
parseRest str =
  let [(t, rest)] = words str
  in
   (read t :: Int, rest)
