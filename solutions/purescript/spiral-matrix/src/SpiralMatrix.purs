module Spiral
  ( spiral
  ) where

import Prelude
import Data.Maybe (fromMaybe)
import Data.List (List(..), fromFoldable, index, modifyAt, updateAt)
import Data.List.Lazy (replicate)
import Data.Tuple (Tuple(..))

data Direction = Right | Down | Left | Up

adjustPosition :: Tuple Int Int -> Direction -> Tuple Int Int
adjustPosition (Tuple row col) dir = case dir of
  Right -> Tuple row (col + 1)
  Down -> Tuple (row + 1) col
  Left -> Tuple row (col - 1)
  Up -> Tuple (row - 1) col

nextDirection :: Direction -> Direction
nextDirection dir = case dir of
  Right -> Down
  Down -> Left
  Left -> Up
  Up -> Right

validDirection :: List (List Int) -> Int -> Int -> Direction -> Int -> Boolean
validDirection matrix row col dir size =
  let
    (Tuple testRow testCol) = adjustPosition (Tuple row col) dir
  in
    testRow >= 0 && testRow < size &&        
    testCol >= 0 && testCol < size &&    
    fromMaybe 1 (index (fromMaybe Nil (index matrix testRow)) testCol) == 0
    
fill :: List (List Int) -> Tuple Int Int -> Direction -> Int -> Int ->List (List Int)
fill matrix _pos _dir n size | n > (size * size) = matrix
fill matrix (Tuple row col) dir n size =
  let
    newMatrix = fromMaybe Nil (modifyAt row (\r -> fromMaybe Nil (updateAt col n r)) matrix)
    nextDir = if validDirection matrix row col dir size then dir else nextDirection dir
    nextPos = adjustPosition (Tuple row col) nextDir
  in
    fill newMatrix nextPos nextDir (n + 1) size
      

-- spiral returns a square matrix of a given size that is filled with natural numbers,
-- from 1 in the top-left corner and increasing in an inward, clockwise spiral order.
spiral :: Int -> List (List Int)
spiral size = 
  let
    initial = fromFoldable (replicate size (fromFoldable (replicate size 0)))
  in
    fill initial (Tuple 0 0) Right 1 size
