module FoodChain (song) where

import Data.List (intercalate)

chorus :: String -> String
chorus animal = case animal of
  "fly" -> "a fly.\n"
  "spider" -> "a spider.\nIt wriggled and jiggled and tickled inside her.\n"
  "bird" -> "a bird.\nHow absurd to swallow a bird!\n"
  "cat" -> "a cat.\nImagine that, to swallow a cat!\n"
  "dog" -> "a dog.\nWhat a hog, to swallow a dog!\n"
  "goat" -> "a goat.\nJust opened her throat and swallowed a goat!\n"
  "cow" -> "a cow.\nI don't know how she swallowed a cow!\n"
  "horse" -> "a horse.\nShe's dead, of course!\n"
  _ -> ""

finale :: String -> String
finale animal = case animal of
  "fly" -> "I don't know why she swallowed the fly. Perhaps she'll die.\n"
  "spider" -> "She swallowed the spider to catch the fly.\n"
  "bird" -> "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n"
  "cat" -> "She swallowed the cat to catch the bird.\n"
  "dog" -> "She swallowed the dog to catch the cat.\n"
  "goat" -> "She swallowed the goat to catch the dog.\n"
  "cow" -> "She swallowed the cow to catch the goat.\n"
  _ -> ""

toCatch :: String -> String
toCatch "horse" = ""
toCatch animal = got animal ""
  where
    got "" acc = acc -- base case
    got current acc = got previous (acc ++ finale current)
      where
        previous = case current of
          "fly" -> ""
          "spider" -> "fly"
          "bird" -> "spider"
          "cat" -> "bird"
          "dog" -> "cat"
          "goat" -> "dog"
          "cow" -> "goat"
          _ -> ""

verse :: String -> String
verse animal = concat ["I know an old lady who swallowed ", chorus animal, toCatch animal]

song :: String
song = intercalate "\n" (map verse animal)
  where
    animal = ["fly", "spider", "bird", "cat", "dog", "goat", "cow", "horse"]