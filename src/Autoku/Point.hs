
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
