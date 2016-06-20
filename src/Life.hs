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
countNeighbors grid = foldl' addTo Map.empty points
  where addTo map point = Map.insertWith (+) point 1 map
        points = toList grid >>= neighbors

-- | Run a single step, killing and birthing cells as needed.
executeStep :: Grid -> Step -> Grid
executeStep prev step = Map.keysSet $ Map.filterWithKey isLive step
  where isLive point neighbors
          | Set.member point prev = neighbors == 2 || neighbors == 3
          | otherwise             = neighbors == 3

-- | Run a whole step in the game of life.
step :: Grid -> Grid
step prev = executeStep prev (countNeighbors prev)

-- | The entire evolution of a game of life from the given initial
-- configuration.
steps :: Grid -> [Grid]
steps = iterate step

-- * Interesting Patterns

-- | The r-pentomino only has five live cells but results in a lot of
-- activity. This pattern centers the rPentomino in a grid ranging
-- from '(0, 0)' to '(100, 100)'.
rPentomino :: Grid
rPentomino = Set.fromList [(24, 25), (25, 24), (25, 25), (25, 26), (26, 24)]
