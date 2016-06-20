{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}

--------------------------------------------------------------------------------
-- |
-- Module: Game
--
-- This is a very simple implementation of the rules of John Conway's
-- Game of Life. It uses `Repa' arrays and `Stencil' functionality.
--
-- This is based on slides at
-- <http://illustratedhaskell.org/index.php/2011/09/24/conways-game-of-life-with-repa/>

module Game where

import           Data.Array.Repa              ((:.) (..), Z (..), (!))
import qualified Data.Array.Repa              as R
import           Data.Array.Repa.Stencil      (Boundary (..), Stencil)
import           Data.Array.Repa.Stencil.Dim2

type Grid = R.Array R.U R.DIM2 Int

-- | A stencil that calculates a cell's neighbor count.
neighbors :: Stencil R.DIM2 Int
neighbors = [stencil2|1 1 1
                      1 0 1
                      1 1 1|]

-- | A blank width × height grid.
blank :: Int -> Int -> Grid
blank width height = R.fromListUnboxed (Z :. width :. height) $ replicate (width * height) 0

-- | Represents the rules of the game of life for each cell.
transition :: Int -> Int -> Int
transition 1 2 = 1
transition 1 3 = 1
transition 1 _ = 0
transition 0 3 = 1
transition 0 _ = 0

-- | Represents a single generation of life.
step :: Grid -> Grid
step grid = R.computeUnboxedS . R.zipWith transition grid $ mapStencil2 (BoundConst 0) neighbors grid

-- | Flips the value at the given point. If the value is 0, it becomes
-- 1; otherwise, it becomes 0.
modify :: (Int, Int) -> Grid -> Grid
modify p grid = R.computeUnboxedS $ R.fromFunction (R.extent grid) go
  where go i@(Z :. x' :. y') | p == (x', y') = if grid ! i == 0 then 1 else 0
                             | otherwise    = grid ! i

-- | Returns a list of points corresponding to which pixel should be drawn.
render :: Grid -> [(Int, Int)]
render grid = [(x, y) | x <- [0..width - 1], y <- [0..height - 1], grid ! (Z :. x :. y) /= 0]
  where Z :. width :. height = R.extent grid


-- ** Interesting Start States

-- | The r pentomino is a "methuselah", a simple pattern that results
-- in a lot of activity—1103 generations from *five* starting cells!
rPentomino :: Grid
rPentomino = foldr modify (blank 400 400) [(99, 100), (100, 99), (100, 100), (100, 101), (101, 99)]

-- | Starts a glider in the top lefhand corner that moves towards the
-- bottom righthand corner.
glider :: Grid
glider = foldr modify (blank 400 400) [(2, 0), (3, 1), (1, 2), (2, 2), (3, 2)]
