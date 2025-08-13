module Acronym (abbreviate) where

import Data.Char

abbreviate :: String -> String
abbreviate = map (toUpper . head) . words . splitWords

splitWords :: String -> String
splitWords (x : y : xs)
  | isLower x && isUpper y = x : ' ' : y : splitWords xs
splitWords ('_' : xs) = xs
splitWords ('-' : xs) = ' ' : xs
splitWords (x : xs) = x : splitWords xs
splitWords [] = []
