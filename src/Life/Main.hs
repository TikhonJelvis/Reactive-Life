module Main where

import           Control.Applicative ((<*>))
import           Control.Arrow       (second, (***))

import           Data.Array.Repa     ((:.) (..), Z (..), extent, (!))

import           Graphics.UI.WX      hiding (Event)

import           Reactive.Banana
import           Reactive.Banana.WX

import           Life.Game

main :: IO ()
main = start $ do
  mw          <- frame     [text := "John Conway's Game of Life", resizeable := False]
  lifeTimer   <- timer  mw [interval := 50]
  lifePanel   <- panel  mw [bgcolor := white]
  pauseButton <- button mw [text := "▶"]

  let pos width height control = minsize (sz width height) $ widget control
  set mw [layout := column 2 [pos 40 25 pauseButton, pos 800 800 lifePanel]]

  let network =
        do time   <- event0   lifeTimer   command
           pauses <- event0   pauseButton command
           clicks <- event1   lifePanel   click
           let active  = accumB False $ not <$ pauses
               changes = whenE active (step <$ time) `union` (modify . toPt <$> clicks)
               life    = accumB (blank 200 200) changes
           sink lifePanel [on paint :== renderLife <$> life]
           reactimate $ repaint lifePanel <$ changes
           sink pauseButton [text :== (\ t -> if t then "❚❚" else "▶") <$> active]

  compile network >>= actuate
  where toPt (Point x y) = (x, y)

renderLife :: LifeGrid -> DC a -> Rect -> IO ()
renderLife grid ctx _ =
  sequence_ $ [drawPx (x) (y) | x <- [0..width - 1], y <- [0..height - 1], grid ! (Z :. x :. y) == 1]
  where zoom = 4
        Z :. width :. height = extent grid
        drawPx x y = drawRect ctx (Rect (x * zoom) (y * zoom) zoom zoom) [bgcolor := black]
