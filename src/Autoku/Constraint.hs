module Autoku.Constraint where

import           Data.Maybe
import           Text.Printf

import           Autoku.Cell
import           Autoku.Point


data Constraint = Constraint
  { cName     :: String
  , cPoints   :: Maybe [Point]
  , cOthers   :: Point -> [Point]
  , cRestrict :: [Cell] -> Cell -> Cell
  }

instance Show Constraint where
  show = cName


applies :: Point -> Constraint -> Bool
applies p c = maybe True (elem p) $ cPoints c


kingsRule :: Constraint
kingsRule = Constraint
  { cName     = "king's rule"
  , cPoints   = Nothing
  , cOthers   = kingNeighbours
  , cRestrict = \cs c -> foldr restrict c $ mapMaybe value cs
  }

knightsRule :: Constraint
knightsRule = Constraint
  { cName     = "knight's rule"
  , cPoints   = Nothing
  , cOthers   = knightNeighbours
  , cRestrict = \cs c -> foldr restrict c $ mapMaybe value cs
  }

knightSumRule :: Int -> Point -> Constraint
knightSumRule x p = Constraint
  { cName     = printf "sum of knight neighbours of %s is $d" (show p) x
  , cPoints   = Just [p]
  , cOthers   = const $ knightNeighbours p
  , cRestrict = \cs -> intersect $ (x-) . sum <$> traverse possible cs
  }
