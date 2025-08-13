module DND
  ( Character (..),
    ability,
    modifier,
    character,
  )
where

import Test.QuickCheck (Gen)
import Test.QuickCheck.Gen (choose)

data Character = Character
  { strength :: Int,
    dexterity :: Int,
    constitution :: Int,
    intelligence :: Int,
    wisdom :: Int,
    charisma :: Int,
    hitpoints :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier x = div (x - 10) 2

ability :: Gen Int
ability = do
  rolls <- sequence $ replicate 4 (choose (1, 6))
  let m = minimum rolls
      s = sum rolls
  return $ s - m

character :: Gen Character
character = do
  str <- ability
  dex <- ability
  con <- ability
  int <- ability
  wis <- ability
  chr <- ability
  return $ Character str dex con int wis chr $ 10 + modifier con
