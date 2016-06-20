{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Monad                 (forM_, void)

import           Data.Foldable                 (toList)
import qualified Data.Set                      as Set

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Canvas (Canvas)
import qualified Graphics.UI.Threepenny.Canvas as Canvas
import           Graphics.UI.Threepenny.Core

import qualified Reactive.Threepenny           as UI

import           Game

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = void $ do
  let Options {..} = def
  return w # set title "A Reactive Game of Life"

  canvas      <- lifeCanvas

  timer       <- UI.timer # set UI.interval 200
  let updates = step <$ UI.tick timer

  mouse <- mouse canvas
  let adjust (x, y) = (x `div` scale, y `div` scale)
  let modifies      = modify . adjust <$> mouse <@ UI.click canvas
  let doAll         = foldr (.) id
  let changes       = doAll <$> UI.unions [updates, modifies]

  grids <- UI.accumE rPentomino changes

  registerUpdate canvas grids
  timer # UI.start
  getBody w #+ [return canvas]

-- | The mouse position, in pixel coordinates relative to the top
-- lefthand corner of the given element.
mouse :: Element -> UI (Behavior (Int, Int))
mouse element = accumB (0, 0) $ const <$> UI.mousemove element

data Options = Options { -- | the size of the grid, measured in cells (not pixels!)
                         size  :: (Int, Int)
                       , -- | the size, in pixels, of each cell
                         scale :: Int
                       , -- | the color of the life cells
                         color :: Canvas.FillStyle
                       }

-- | Some sensible default options for rendering a game of life grid.
def :: Options
def = Options { size   = (400, 400)
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
                # set UI.width (width * scale)
                # set UI.height (height * scale)
  forM_ (render grid) $ \ (x, y) -> do
    Canvas.fillRect (pt (x * scale, y * scale)) cell cell canvas
  where Options { scale, size = (width, height) } = options
        cell   = fromIntegral scale

registerUpdate :: Canvas -> Event Grid -> UI ()
registerUpdate canvas steps = do
  window <- askWindow
  liftIO . void . register steps $ \ grid -> do
    runUI window $ drawGrid def grid canvas
