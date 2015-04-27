--------- Custom Data Types ---

--- Type Synonyms

--- type synonyms allow you to do c++ typedefs in haskell

type String = [Char] --

type Point = (Double, Double)

midPoint :: (Double, Double) -> (Double, Double) -> (Double, Double)
midPoint (x1,y1) (x2,y2) =
  ( (x1 + x2) / 2, (y1 + y2) / 2 )

-- makes it more clear what's happening
midPoint :: Point -> Point -> Point
midPoint (x1,y1) (x2,y2) =
  ( (x1 + x2) / 2, (y1 + y2) / 2 )

-- type synonyms are completely interchangeable with
-- the types they represent

p1::(Double,Double)
p1 = (1,2)

p2::Point
p2 = (3,4)

mid::(Double, Double)
mid = midpoint p1 p2

--- NEWTYPE ---
-- not interchangeable
newtype CustomerId = MakeCustomerId Int
--MakeCustomerId is the constructor name for CustomerId

customer :: CustomerId
customer = MakeCustomer 13

--extract int from CustomerId
-- using pattern matching
customerToInt :: CustomerId -> Int
customerToInt (MakeCustomerId i) = i

--it's common to make the constructor name the same as the
-- type
newtype CustomerId = CustomerId Int

customer :: CustomerId
customer = CustomerId 13

customerToInt :: CustomerId -> Int
customerToInt (CustomerId i) = i


--- RECORDS (one of the weakest points in the language,
--    and should generally be avoided)---
-- data keyword, followed by type, then constructor name
-- then field definitions

data Customer = MakeCustomer
  { customerId :: CustomerId
  , name :: String -- good haskell comma form
  , luckyNumber :: Int
  }

alice :: Customer
alice = MakeCustomer
  { customerId = MakeCustomer 13
  , name = "Alice"
  , luckyNumber = 69 --Alice, you freak
  }

-- accessing alice record data
customerId alice -- Result: MakeCustomerId 13
name alice -- Result: "Alice"
luckyNumber alice -- Result: 69

-- Records can be updated
-- means creating a new record from old data and new data

sally = alice {name = "Sally", luckyNumber = 84}

-- accessing sally record data
customerId sally -- Result: MakeCustomerId 13
name sally -- Result: "Sally"
luckyNumber sally -- Result: 84

-- RECORDS ARE FLAWED!!!! WHY?!
---- Not extensible
--DOESN"T WORK
data Person = Person {name::String}
data Customer = Customer extends Person {luckyNumber :: Int}

----No Shared Field Names (seriously... wowzer)
data Customer = Customer { name::String {-|same-}, customerId :: CustomerId }
data Supplier = Supplier { name::String {-|same-}, supplierId :: SupplierId }
--both have name definition, so it breaks... wow that sucks


-- ALGEBRAIC DATA TYPES
---- Workhorse of haskell types

-- data keyword, name of type, = constructor name, type of args to constructor
data Customer = MakeCustomer CustomerId String Int

--common pattern in haskell to name type and constructor the same thing
data Customer = Customer CustomerId String Int

--newtype
newtype CustomerId = CustomerId Int

-- NOTE: algebraic data types and newtype only differ by
--       the amount of arguments you can create them with.
--       newtype -> only 1 constructor argument
--       adt -> multiple constructor arguments

--Create algebraic type
alice::Customer
alice = Customer (CustomerId 13) "Alice" 69

--extrac value from adt using pattern matching
getCustomerId :: Customer -> CustomerId
getCustomerId (Customer cust_id name luckyNumber {-|pattern matches a customer|-})
  = cust_id

--easier to wildcard parts you don't care about
getCustomerId :: Customer -> CustomerId
getCustomerId (Customer cust_id _ _) = cust_id

-- a tree structure
data StringTree = StringTree String [StringTree]

--example of tree
heirarchy = StringTree "C:"
            [ StringTree "Program Files" []
            , StringTree "Users"
               [StringTree "Alice" []]
            , StringTree "Cats" []
            ]

-- ADT can have multiple constructors
data bool = False | True

x::Bool
x = False
y::Bool
y = True

-- pattern matching
negate :: Bool -> Bool
negate True = False
negate False = True

-- haskell enum equivalent
data DialogResponse = Yes | No | Help | Quit

-- nullable equivalent in haskell
data MaybeInt = NoInt | JustInt Int

defaultInt :: Int -> MaybeInt -> Int
defaultInt defaultValue NoInt = defaultValue -- NoInt so use default
defaultInt _ (JustInt x) = x -- MaybeInt with value, so return that

let val = defaultInt 10 NoInt -- result 10
let val = defaultInt 10 (JustInt 5) -- result 5

data StringList = EmptyStringList
                  | ConsStringList String {-|head|-} StringList {-|tail|-}

lengthStringList :: StringList -> Int
lengthStringList EmptyStringList = 0
lengthStringList (ConsStringList _ rest) = 1 + length rest


-- Parameterized Types --

--Maybe from std lib
data Maybe a = Just a | Nothing
-- this means that when supplied a type, you get a Maybe type back

x :: Maybe Int
x = Nothing

--takes default value of a, and a maybe type of a and returns a
fromMaybe :: a -> Maybe a -> a
fromMaybe defaultVal Nothing = defaultVal
fromMaybe _ (Just x) = x

--std list type
data List a = Empty | Cons a (List a)
--Cons take head of list and then tail as args

--Parameterized type can also take multiple type parameters
data Map k a = ...

-- NOTE: Parameterized Type allow you to create types that can
--       hold any other type.  They hold values of any type.
--       Like generic lists and dictionaries

-- Type Class Instances

elem _ [] = False -- handle empty case
elem x (y : ys)   -- handle list
  | x == y    = True -- head == target
  | otherwise = elem x ys -- recurse to continue search
  
  
