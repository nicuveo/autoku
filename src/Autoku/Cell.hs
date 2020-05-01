module Autoku.Cell where

import           Control.Exception
import           Control.Monad
import qualified Data.List         as L
import           Data.Maybe


type Cell = [Int]


empty :: Cell
empty = [1..9]

solved :: Int -> Cell
solved = pure . join (assert . (`elem` empty))



isSolved :: Cell -> Bool
isSolved = isJust . value

value :: Cell -> Maybe Int
value [x] = Just x
value _   = Nothing

canBeSolved :: Cell -> Bool
canBeSolved = not . null

possible :: Cell -> [Int]
possible = id


restrict :: Int -> Cell -> Cell
restrict = L.delete

intersect :: [Int] -> Cell -> Cell
intersect = L.intersect
