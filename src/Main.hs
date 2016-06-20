{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           Control.Monad                 (forM_, void)

import           Data.Foldable                 (toList)
import qualified Data.Set                      as Set

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Canvas (Canvas)
import qualified Graphics.UI.Threepenny.Canvas as Canvas
import           Graphics.UI.Threepenny.Core

import qualified Reactive.Threepenny           as UI

import           Life

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = void $ do
  return w # set title "A Reactive Game of Life"
  canvas <- lifeCanvas
  drawGrid def rPentomino canvas
  timer <- UI.timer # set UI.interval 200
  let updates = step <$ UI.tick timer
  grids <- UI.accumE rPentomino updates
  registerUpdate canvas grids
  timer # UI.start
  getBody w #+ [return canvas]

data Options = Options { -- | the bounds, measured in life cells (not pixels!)
                         bounds :: ((Int, Int), (Int, Int))
                       , -- | the size, in pixels, of each cell
                         scale  :: Int
                       , -- | the color of the life cells
                         color  :: Canvas.FillStyle
                       }

-- | Some sensible default options for rendering a game of life grid.
def :: Options
def = Options { bounds = ((-200, -200), (200, 200))
              , scale  = 5
              , color  = Canvas.htmlColor "black"
              }

pt :: (Int, Int) -> Canvas.Point
pt (x, y) = (fromIntegral x, fromIntegral y)

lifeCanvas :: UI Element
lifeCanvas = mkElement "canvas" #. "reactive-life"

-- | Draw the portion of the given game of life grid using the
-- specified rendering options (which control how much of the grid to
-- draw and at what scale).
drawGrid :: Options -> Grid -> Canvas -> UI ()
drawGrid options grid canvas = do
  Canvas.clearCanvas canvas
  return canvas # set Canvas.fillStyle (color options)
                # set UI.width width
                # set UI.height height
  forM_ points $ \ (x, y) -> do
    Canvas.fillRect (pt' (x * scale, y * scale)) cell cell canvas
  where Options { scale, bounds = ((x₁, y₁), (x₂, y₂)) } = options
        width  = fromIntegral $ (x₂ - x₁) * scale
        height = fromIntegral $ (y₂ - y₁) * scale
        cell   = fromIntegral scale

        pt' (x, y) = pt (x - x₁, y - y₁)

        points = toList $ Set.filter within grid
        within (x, y) = x >= x₁ && x <= x₂ && y >= y₁ && y <= y₂

registerUpdate :: Canvas -> Event Grid -> UI ()
registerUpdate canvas steps = do
  window <- askWindow
  liftIO . void . register steps $ \ grid -> do
    runUI window $ drawGrid def grid canvas
