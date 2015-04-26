--- Custom Data Types ---

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
