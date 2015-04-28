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

--Eq is required for elem to know how to compare
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False -- handle empty case
elem x (y : ys)   -- handle list
  | x == y    = True -- head == target
  | otherwise = elem x ys -- recurse to continue search


data RGB = RGB Int Int Int

colors = [RBG 255 0 0, RGB 0 255 0, RGB 0 0 255]
green = RGB 0 255 0
greenInColors = elem green colors -- this won't work because RBG is
                                  --  not in the Eq Type Class

-- to put RGB in the Eq TypeClass we have to use instance
instance Eq RGB where
  (RGB r1 g1 b1) == (RGB r2 g2 b2) =
    (r1 == r2) && (g1 == g2) && (b1 == b2)

show RGB -- Doesn't work because it doesn't know how to represent
         -- itself as a string

instance Show RGB where
  show (RGB r g b) =
    "RGB " ++ (show r) ++ " " ++ (show g) ++ " " ++ (show b)

-- Type Class Instances for Parameterized Types
data Maybe' a = Nothing' | Just' a

--instance with a context
instance (Eq a) => Eq (Maybe' a) where
  Nothing' == Nothing' = True
  Nothing' == (Just' _) = False
  (Just' _) == Nothing' = False
  (Just' x) == (Just' y) = x == y

-- DERIVING TYPE CLASS INSTANCES

data RGB = RGB Int Int Int

instance Eq RGB where
  (RGB r1 g1 b1) == (RGB r2 g2 b2) =
    (r1 == r2) && (g1 == g2) && (b1 == b2)

data Person = Person String Int Int

instance Eq Person where
  (Person name1 age1 height1) ==
    (Person name2 age2 height2) =
      (name1 == name2) && (age1 == age2) && (height1 == height2)

-- Eq instance gets tedious

data RGB = RGB Int Int Int
  deriving Eq -- gives obvious comparison method

data Person = Person String Int Int
  deriving Eq

--Useful Deriving Types
-- *Eq
-- -> Deriving - component-wise equality
-- *Ord
-- -> (<),(>),(<=), (>=)
-- -> Deriving - component-wise comparison
-- *Show
-- -> show
-- -> Deriving -"{Constructor-Name} {arg1} {arg2} ..."
-- *Read
-- -> read
-- -> Deriving - parse output of default show

-- DEFINING Type Classes

--Eq implementation
class Eq a where
  (==) :: a -> a -> Bool --equal
  (/=) :: a -> a -> Bool --not equal

data Point2 = Point2 Double Double

distance2 :: Point2->Point2->Double
distance2 (Point2 x1 y1) (Point2 x2 y2)
 sqrt (dx * dx + dy * dy)
   where dx = x1 - x2
         dy = y1 - y2

data Point3 = Point3 Double Double Double

distance3 :: Point3 -> Point3 -> Double
distance3 (Point3 x1 y1 z1) (Point3 x2 y2 z2)
  sqrt (dx*dx+dy*dy+dz*dz)
  where dx = x1 - x2
        dy = y1 - y2
        dz = z1 - z2

pathLength2 :: [Point2] -> Double
pathLength2 num = case num of
  []         -> 0
  (_:[])     -> 0
  (p1:p2:ps) -> distance2 p1 p2 + pathLength2 p1:ps

pathLength3 :: [Point3] -> Double
pathLength3 num = case num of
  []         -> 0
  (_:[])     -> 0
  (p1:p2:ps) -> distance3 p1 p2 + pathLength p1:ps

class Measurable a where
  distance :: a -> a -> Double

instance Measurable Point2 where
  distance = distance2

instance Measurable Point3 where
  distance = distance3

--Now we can write a polymorphic pathlength function
--  that works for both Point2 and Point3

pathLength :: Measurable a => [a] -> Double
pathLength2 num = case num of
  []         -> 0
  (_:[])     -> 0
  (p1:p2:ps) -> distance p1 p2 + pathLength2 p1:ps

-- Subclasses of Type Classes

data Point2 = Point2 Double Double deriving Show
data Point3 = Point3 Double Double Double deriving Show

class Measurable a where
  distance :: a -> a -> Double

-- Make Directions a subclass of both Measurable and Show
-- this means that a must be of type Measurable and Show
class (Measurable a, Show a) => Directions a where
  getDirections :: a -> a -> String
  getDirections p1 p2 =
    "Go from " ++ (show p1) ++ --showable
    " towards" ++ (show p2) ++
    " and stop after " ++ (show (distance p1 p2)) -- and measurable

instance Directions Point3 where
  getDirections p1 p2 =
    "Fly from " ++ (show p1) ++ --showable
    " towards" ++ (show p2) ++
    " and stop after " ++ (show (distance p1 p2)) -- and measurable

instance Direction Point2 where -- empty declaration required

---------------- SUMMARY ----------------

-- Instances
-- Deriving
-- Defining type classes
-- Subclasses
