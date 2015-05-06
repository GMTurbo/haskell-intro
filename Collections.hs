
-- Functional Collections --

data List a = Empty | Cons a (List a)

-- immutable

-- updates first element of collection
-- but really creates a new list with y as the first element
updateFirst :: List a -> a -> List a
updateFirst Empty y = Empty
updateFirst (Cons x xs) y = Cons y xs

-- Modification -> Copy (seems slow)
-- Immutable data can be shared (always safe to share data)
-- Copy -> use reference to original (fast)


-------- Set --------
-- Unordered collection of unique values

import Data.Set

empty :: Set a -- set of any type
insert :: a -> Set a -> Set a
delete :: a -> Set a -> Set a
union :: Set a -> Set a -> Set a
member :: a -> Set a -> Bool

-- Set Restrictions --
-- Set of functions?
-- -- NO, functions are not in the Ord Type Class
-- Sets only work with types in the Ord Type class (inherits Eq TC)

-- So Really...
empty :: Set a -- set of any type
insert :: Ord a => a -> Set a -> Set a
delete :: Ord a => a -> Set a -> Set a
union :: Ord a => Set a -> Set a -> Set a
member :: Ord a => a -> Set a -> Bool

-------- Map --------
-- Key-value pair collection

import qualified Data.Map

data Map k a -- represents key value collection
             --  k = key type
             --  a = value type

empty :: Map k a
-- if key exists, it'll be overwritten
insert :: Ord k => k -> a -> Map k a -> Map k a
-- if key exists, it's deleted, otherwise return original map
delete :: Ord k => k -> Map k a -> Map k a
union :: Ord k => Map k a -> Map k a -> Map k a
-- given key and map, returns a maybe
lookup :: Ord k => k -> Map k a -> Maybe a


-------- Seq --------
-- Ordered List

import Data.Sequence

empty :: Seq a

(<|) :: a -> Seq a -> Seq a -- exactly the same a : for Lists
(|>) :: Seq a -> a -> Seq a -- add element to the end of the seq
(><) :: Seq a -> Seq a -> Seq a -- concatinate two seq together

-------- Seq Pattern Matching --------

------ View Patterns ------
-- View patterns can be used to deconstruct Seqs

-- turn them on
{-# Language ViewPatterns #-}
-- or
--:set -XViewPatterns

length :: Seq a -> Int
length (viewl -> EmptyL) = 0
length (viewl -> x :< xs) = 1 + length xs

viewl :: Seq a -> ViewL a

data ViewL a
  = EmptyL
  | a :< (Seq a)

length' :: Seq a -> Int
length' (viewr -> EmptyR) = 0
length' (viewr -> xs :> x) = 1 + length xs

viewl :: Seq a -> ViewL a

data ViewR a
  = EmptyR
  | a :> (Seq a)

-- Seq usually faster than list

------ Summary ------
-- Purely functional collections
-- Set -> unordered collection of unique values
-- Map -> key/value pairs
-- Seq -> A super powered list (requires view patterns)
