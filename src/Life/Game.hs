{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}

--------------------------------------------------------------------------------
-- |
-- Module: Life.Game
-- 
-- This is a very simple implementation of the rules of John Conway's
-- Game of Life. It uses `Repa' arrays and `Stencil' functionality.
-- 
-- This is based on slides at
-- <http://illustratedhaskell.org/index.php/2011/09/24/conways-game-of-life-with-repa/>

module Life.Game where

import           Data.Array.Repa              ((:.) (..), Z (..), (!))
import qualified Data.Array.Repa              as R
import           Data.Array.Repa.Stencil      (Boundary (..), Stencil)
import           Data.Array.Repa.Stencil.Dim2

type LifeGrid = R.Array R.U R.DIM2 Int

-- | A stencil that calculates a cell's neighbor count.
neighbors :: Stencil R.DIM2 Int
neighbors = [stencil2|1 1 1
                      1 0 1
                      1 1 1|]

-- | A blank width × height grid.
blank :: Int -> Int -> Life
blank width height = R.fromListUnboxed (Z :. width :. height) $ replicate (width * height) 0

-- | Represents the rules of the game of life for each cell.
transition :: Int -> Int -> Int
transition 1 2 = 1
transition 1 3 = 1
transition 1 _ = 0
transition 0 3 = 1
transition 0 _ = 0

-- | Represents a single generation of life.
step :: Life -> Life
step grid = R.computeUnboxedS . R.zipWith transition grid $ mapStencil2 (BoundConst 0) neighbors grid

-- | Flips the value at the given point. If the value is 0, it becomes
-- 1; otherwise, it becomes 0.
modify :: (Int, Int) -> Life -> Life
modify p grid = R.computeUnboxedS $ R.fromFunction (R.extent grid) go
  where go i@(Z :. x' :. y') | p == (x', y') = if grid ! i == 0 then 1 else 0
                             | otherwise    = grid ! i

-- | Returns a list of points corresponding to which pixel should be drawn.
render :: Life -> [(Int, Int)]
render grid = [(x, y) | x <- [0..width - 1], y <- [0..height - 1], grid ! (Z :. x :. y) /= 0]
  where Z :. width :. height = R.extent grid
