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
  lifeTimer   <- timer      mw [interval := 50]
  lifePanel   <- panel      mw [bgcolor := white]
  pauseButton <- button     mw [text := "▶"]
  clearButton <- button     mw [text := "✘"]
  genLabel    <- staticText mw [text := "generation 0"]
  speedSlider <- hslider    mw False 0 100 [selection := 70]

  let pos width height control = minsize (sz width height) $ widget control
  set mw [layout := column 2 [row 5 [pos 50 25 pauseButton, pos 50 25 clearButton,
                                     pos 100 25 speedSlider, floatRight $ pos 125 20 genLabel],
                              pos 800 800 lifePanel]]

  let network =
        do time    <- event0 lifeTimer   command
           pauses  <- event0 pauseButton command
           clears  <- event0 clearButton command
           clicks  <- event1 lifePanel   mouse
           slides  <- event0 speedSlider command
           let active  = accumB False $ not <$ pauses
               changes = unions [whenE active ((succ *** step) <$ time), modifyGrid <$> clicks,
                                 const (0, blank 200 200) <$ clears]
               life    = accumB (0, blank 200 200) changes
           sink lifePanel [on paint :== renderLife . snd <$> life]
           reactimate $ repaint lifePanel <$ changes
           sink pauseButton [text :== (\ t -> if t then "❚❚" else "▶") <$> active]
           sink genLabel [text :== ("generation: " ++) . show . fst <$> life]
           let updateTimer n = set lifeTimer [interval := (100 - n) * 3 + 10]
           reactimate $ (get speedSlider selection >>= updateTimer) <$ slides

  compile network >>= actuate
  where modifyGrid (MouseLeftDown (Point x y) _) = second $ modify (x `div` 4, y `div` 4)
        modifyGrid (MouseLeftDrag (Point x y) _) = second $ setPx (x `div` 4, y `div` 4) True
        modifyGrid _                             = id

renderLife :: LifeGrid -> DC a -> Rect -> IO ()
renderLife grid ctx _ = sequence_ $ [drawPx x y | x <- [0..width - 1], y <- [0..height - 1], grid ! (Z :. x :. y) == 1]
  where Z :. width :. height = extent grid
        drawPx x y = drawRect ctx (Rect (x * 4) (y * 4) 4 4) [bgcolor := black]
