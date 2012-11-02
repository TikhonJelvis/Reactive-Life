module Main where

import           Control.Arrow      ((***), second)

import           Data.Array.Repa    ((:.) (..), Z (..), extent, (!))

import           Graphics.UI.WX     hiding (Event)

import           Reactive.Banana
import           Reactive.Banana.WX

import           Life.Game

main :: IO ()
main = start $ do
  mw          <- frame         [text := "John Conway's Game of Life", resizeable := False]
  lifePanel   <- panel      mw [bgcolor := white]
  pauseButton <- button     mw [text := "▶"]
  clearButton <- button     mw [text := "✘"]
  lifeTimer   <- timer      mw [interval := 100]
  genLabel    <- staticText mw [text := "generation 0"]

  let pos width height control = minsize (sz width height) $ widget control
  set mw [layout := column 2 [row 3 [pos 50 25 pauseButton, pos 50 25 clearButton,
                                     floatRight $ pos 125 25 genLabel],
                              pos 800 800 lifePanel]]

  let network =
        do time   <- event0 lifeTimer   command
           pauses <- event0 pauseButton command
           clears <- event0 clearButton command
           clicks <- event1 lifePanel   mouse
           let start  = (0, blank 200 200)
               active = accumB False $ not <$ pauses
               life   = accumB start $ whenE active ((succ *** step) <$ time) `union`
                        (modifyGrid <$> clicks) `union` (const start <$ clears)
           sink lifePanel [on paint :== renderLife . snd <$> life]
           reactimate $ repaint lifePanel <$ time
           reactimate $ setText pauseButton . (\ t -> if t then "▶" else "❚❚") <$> active <@ pauses
           reactimate $ setText genLabel . ("generation " ++) . show . fst <$> life <@ time

  compile network >>= actuate
  where setText c t = set c [text := t]
        modifyGrid (MouseLeftDown (Point x y) _) = second $ modify (x `div` 4, y `div` 4)
        modifyGrid _                             = id

renderLife :: LifeGrid -> DC a -> Rect -> IO ()
renderLife grid ctx _ = sequence_ $ [drawPx x y | x <- [0..width - 1], y <- [0..height - 1], grid ! (Z :. x :. y) == 1]
  where Z :. width :. height = extent grid
        drawPx x y = drawRect ctx (Rect (x * 4) (y * 4) 4 4) [bgcolor := black]
