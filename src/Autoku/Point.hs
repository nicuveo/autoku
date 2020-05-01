module Autoku.Point
  ( Point(pRow, pCol)
  , readPoint
  , mkPoint
  , allPoints
  , row
  , column
  , square
  , kingNeighbours
  , knightNeighbours
  , a1,a2,a3,a4,a5,a6,a7,a8,a9
  , b1,b2,b3,b4,b5,b6,b7,b8,b9
  , c1,c2,c3,c4,c5,c6,c7,c8,c9
  , d1,d2,d3,d4,d5,d6,d7,d8,d9
  , e1,e2,e3,e4,e5,e6,e7,e8,e9
  , f1,f2,f3,f4,f5,f6,f7,f8,f9
  , g1,g2,g3,g4,g5,g6,g7,g8,g9
  , h1,h2,h3,h4,h5,h6,h7,h8,h9
  , i1,i2,i3,i4,i5,i6,i7,i8,i9
  ) where


import           Control.Arrow
import           Data.Char
import           Data.Hashable
import           Data.List
import           Data.Maybe
import           Data.String



-- type

data Point = Point
  { pRow :: !Int
  , pCol :: !Int
  } deriving (Eq, Ord)



-- instances

instance Show Point where
  show (Point r c) = [chr (65 + r), intToDigit (1 + c)]

instance Read Point where
  readsPrec _ s = case first readPoint $ splitAt 2 s of
    (Just q, r) -> [(q, r)]
    _           -> []

instance IsString Point where
  fromString = read

instance Enum Point where
  toEnum x = fromJust $ mkPoint (x `div` 9) (x `mod` 9)
  fromEnum (Point r c) = r * 9 + c

instance Bounded Point where
  minBound = Point 0 0
  maxBound = Point 8 8

instance Hashable Point where
  hashWithSalt = hashUsing fromEnum



-- creating a point

readPoint :: String -> Maybe Point
readPoint [r,c] = mkPoint (ord r - 65) (ord c - 49)
readPoint _     = Nothing

mkPoint :: Int -> Int -> Maybe Point
mkPoint r c = listToMaybe [ Point r c
                          | r >= 0 && r <= 8
                          , c >= 0 && c <= 8
                          ]

allPoints :: [Point]
allPoints = [minBound .. maxBound]



-- other points

row :: Point -> [Point]
row (Point r c) = [Point r x | x <- delete c [0..8]]

column :: Point -> [Point]
column (Point r c) = [Point x c | x <- delete r [0..8]]

square :: Point -> [Point]
square p@(Point r c) = delete p [Point (v+y) (u+x) | y <- [0..2], x <- [0..2]]
  where u = 3 * div c 3
        v = 3 * div r 3

kingNeighbours :: Point -> [Point]
kingNeighbours p@(Point r c) = delete p $ Point
  <$> [max 0 (r-1) .. min 8 (r+1)]
  <*> [max 0 (c-1) .. min 8 (c+1)]

knightNeighbours :: Point -> [Point]
knightNeighbours (Point r c) = catMaybes $ zipWith mkPoint
  [r-2, r-2, r-1, r-1, r+1, r+1, r+2, r+2]
  [c-1, c+1, c-2, c+2, c-2, c+2, c-1, c+1]



-- constants

a1,a2,a3,a4,a5,a6,a7,a8,a9 :: Point
b1,b2,b3,b4,b5,b6,b7,b8,b9 :: Point
c1,c2,c3,c4,c5,c6,c7,c8,c9 :: Point
d1,d2,d3,d4,d5,d6,d7,d8,d9 :: Point
e1,e2,e3,e4,e5,e6,e7,e8,e9 :: Point
f1,f2,f3,f4,f5,f6,f7,f8,f9 :: Point
g1,g2,g3,g4,g5,g6,g7,g8,g9 :: Point
h1,h2,h3,h4,h5,h6,h7,h8,h9 :: Point
i1,i2,i3,i4,i5,i6,i7,i8,i9 :: Point
a1 = "A1"
a2 = "A2"
a3 = "A3"
a4 = "A4"
a5 = "A5"
a6 = "A6"
a7 = "A7"
a8 = "A8"
a9 = "A9"
b1 = "B1"
b2 = "B2"
b3 = "B3"
b4 = "B4"
b5 = "B5"
b6 = "B6"
b7 = "B7"
b8 = "B8"
b9 = "B9"
c1 = "C1"
c2 = "C2"
c3 = "C3"
c4 = "C4"
c5 = "C5"
c6 = "C6"
c7 = "C7"
c8 = "C8"
c9 = "C9"
d1 = "D1"
d2 = "D2"
d3 = "D3"
d4 = "D4"
d5 = "D5"
d6 = "D6"
d7 = "D7"
d8 = "D8"
d9 = "D9"
e1 = "E1"
e2 = "E2"
e3 = "E3"
e4 = "E4"
e5 = "E5"
e6 = "E6"
e7 = "E7"
e8 = "E8"
e9 = "E9"
f1 = "F1"
f2 = "F2"
f3 = "F3"
f4 = "F4"
f5 = "F5"
f6 = "F6"
f7 = "F7"
f8 = "F8"
f9 = "F9"
g1 = "G1"
g2 = "G2"
g3 = "G3"
g4 = "G4"
g5 = "G5"
g6 = "G6"
g7 = "G7"
g8 = "G8"
g9 = "G9"
h1 = "H1"
h2 = "H2"
h3 = "H3"
h4 = "H4"
h5 = "H5"
h6 = "H6"
h7 = "H7"
h8 = "H8"
h9 = "H9"
i1 = "I1"
i2 = "I2"
i3 = "I3"
i4 = "I4"
i5 = "I5"
i6 = "I6"
i7 = "I7"
i8 = "I8"
i9 = "I9"
