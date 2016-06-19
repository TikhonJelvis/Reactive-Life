module Life where

import           Data.Foldable (foldl', toList)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Set      (Set)
import qualified Data.Set      as Set

-- | A grid in the game of life is a set of points.
type Grid = Set (Int, Int)

-- | The count of neighbors for every single live cell in a grid and
-- any neighbor of a live cell.
type Step = Map (Int, Int) Int

-- | Returns all the neighbors of the given point.
neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x', y') | x' <- [x - 1, x, x + 1],
                               y' <- [y - 1, y, y + 1],
                               (x', y') /= (x, y)]

-- | Count the neighbors for each live cell (and all *its* neighbors)
-- in a grid.
countNeighbors :: Grid -> Step
countNeighbors grid = foldl' addTo Map.empty grid
  where addTo map point = Map.insertWith (+) point 0 map

-- | Run a single step, killing and birthing cells as needed.
executeStep :: Grid -> Step -> Grid
executeStep = undefined
