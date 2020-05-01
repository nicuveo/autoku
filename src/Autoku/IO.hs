{-# LANGUAGE ParallelListComp #-}

module Autoku.IO where


import           Data.Char
import           Data.List
import           Data.List.Split
import           Text.Printf

import           Autoku.Cell



prettyPrint :: [Cell] -> String
prettyPrint cells = unlines $ header : (' ' <$ header) : renderG groups
  where
    groups = chunksOf 9 $ concat [ [ "   "  ++ renderRow 0 r
                                   , c:"  " ++ renderRow 1 r
                                   , "   "  ++ renderRow 2 r
                                   ]
                                 | r <- chunksOf 9 cells
                                 | c <- ['A'..]
                                 ]
    header     = "     1     2     3     4     5     6     7     8     9  "
    smallHLine = "   ┄┄┄┄┄┼┄┄┄┄┄┼┄┄┄┄┄╫┄┄┄┄┄┼┄┄┄┄┄┼┄┄┄┄┄╫┄┄┄┄┄┼┄┄┄┄┄┼┄┄┄┄┄"
    largeHLine = "   ═════╪═════╪═════╬═════╪═════╪═════╬═════╪═════╪═════"
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
