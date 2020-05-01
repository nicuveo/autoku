module Autoku.IO where


import           Data.Char
import           Data.List
import           Data.List.Split
import           Text.Printf

import           Autoku.Cell



prettyPrint :: [Cell] -> String
prettyPrint cells = unlines $ renderG groups
  where
    groups = chunksOf 9 $ chunksOf 9 cells >>= \r -> [renderRow x r | x <- [0..2]]
    smallHLine = "┄┄┄┄┄┼┄┄┄┄┄┼┄┄┄┄┄╫┄┄┄┄┄┼┄┄┄┄┄┼┄┄┄┄┄╫┄┄┄┄┄┼┄┄┄┄┄┼┄┄┄┄┄"
    largeHLine = "═════╪═════╪═════╬═════╪═════╪═════╬═════╪═════╪═════"
    smallVChar = "┊"
    largeVChar = "║"
    renderG g     = intercalate [largeHLine] $ renderS <$> g
    renderS g     = intercalate [smallHLine] $ chunksOf 3 g
    renderRow k l = intercalate largeVChar $ renderSub k <$> chunksOf 3 l
    renderSub k l = intercalate smallVChar $ cell k <$> l
    cell k c
      | Just x <- value c = case k of
                              1 -> printf " (\ESC[1;32m%d\ESC[0m) " x
                              _ -> "     "
      | c == empty = "     "
      | otherwise = printf " %s " [ if x `elem` c then intToDigit x else ' '
                                  | x <- [3*k+1 .. 3*k+3]
                                  ]
