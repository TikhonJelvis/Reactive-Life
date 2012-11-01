{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}

module Life.Game where

import           Data.Array.Repa                  ((:.) (..), Z (..), (!))
import qualified Data.Array.Repa                  as R
import           Data.Array.Repa.Stencil          (Boundary (..), Stencil)
import           Data.Array.Repa.Stencil.Dim2     (makeStencil2, mapStencil2,
                                                   stencil2)

  -- Based on the slides at
  -- http://illustratedhaskell.org/index.php/2011/09/24/conways-game-of-life-with-repa/

type LifeGrid = R.Array R.U R.DIM2 Int

-- | A stencil that calculates a cell's neighbor count.
neighbors :: Stencil R.DIM2 Int
neighbors = [stencil2|1 1 1
                      1 0 1
                      1 1 1|]

-- | The famous r-pentonimo.
rPentonimo :: LifeGrid
rPentonimo = R.fromListUnboxed (Z :. 3 :. 3) $ [0, 1, 1,
                                                1, 1, 0,
                                                0, 1, 0]

-- | A blank width × height grid.
blank :: Int -> Int -> LifeGrid
blank width height = R.fromListUnboxed (Z :. width :. height) $ replicate (width * height) 0

-- | Center the given pattern in the given grid.
embed :: LifeGrid -> -- ^ the inner pattern; should have smaller dimensions
         LifeGrid -> -- ^ the outer grid that will contain the inner pattern
         LifeGrid   -- ^ the outer grid with the inner pattern centered in it
embed inner outer = R.computeUnboxedS $ R.traverse outer id go
  where Z :. iw :. ih = R.extent inner
        Z :. ow :. oh = R.extent outer
        left   = ow `div` 2 - iw `div` 2
        bottom = oh `div` 2 - ih `div` 2
        contained (Z :. x :. y) = and [left <= x, bottom <= y, left + iw > x, bottom + ih > y]
        go get i@(Z :. x :. y) | contained i = inner ! (Z :. x - left :. y - bottom)
                               | otherwise   = 0

-- | Represents the rules of the game of life for each cell.
transition :: Int -> Int -> Int
transition 1 2 = 1
transition 1 3 = 1
transition 1 _ = 0
transition 0 3 = 1
transition 0 _ = 0

-- | Represents a single generation of life.
step :: LifeGrid -> LifeGrid
step grid = R.computeUnboxedS $ R.zipWith transition grid neighborCount
  where neighborCount = mapStencil2 (BoundConst 0) neighbors grid

modify :: (Int, Int) -> LifeGrid -> LifeGrid
modify (x, y) grid = R.computeUnboxedS $ R.fromFunction (R.extent grid) go
  where go i@(Z :. x' :. y') | x' == x && y' == y = if grid ! i == 0 then 1 else 0
                             | otherwise       = grid ! i
