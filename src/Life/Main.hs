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
  mw          <- frame         [text := "John Conway's Game of Life", resizeable := False]
  lifeTimer   <- timer      mw [interval := 50]
  lifePanel   <- panel      mw [bgcolor := white]
  pauseButton <- button     mw [text := "▶"]
  stepButton  <- button     mw [text := "⇥"]
  clearButton <- button     mw [text := "✘"]
  genLabel    <- staticText mw [text := "generation 0"]
  speedSlider <- hslider    mw False 0 100 [selection := 70]
  zoomSlider  <- hslider    mw False 1 8 [selection := 4]

  let pos width height control = minsize (sz width height) $ widget control
  set mw [layout := column 2 [row 7 [pos 40 25 pauseButton, pos 30 25 stepButton,
                                     pos 30 25 clearButton, pos 100 25 speedSlider,
                                     pos 100 25 zoomSlider, floatRight $ pos 125 20 genLabel],
                              pos 800 800 lifePanel]]

  let network =
        do time   <- event0   lifeTimer   command
           pauses <- event0   pauseButton command
           steps  <- event0   stepButton  command
           clears <- event0   clearButton command
           clicks <- event1   lifePanel   mouse
           slides <- event0   speedSlider command
           zoom   <- behavior zoomSlider  selection
           zooms  <- event0   zoomSlider  command
           let active  = accumB False $ not <$ pauses
               changes = unions [whenE active ((succ *** step) <$ time),
                                 (succ *** step) <$ steps,
                                 uncurry modifyGrid <$> (((,) <$> zoom) <@> clicks),
                                 const (0, blank 800 800) <$ clears]
               life    = accumB (0, blank 800 800) changes
           sink lifePanel [on paint :== renderLife <$> zoom <*> (snd <$> life)]
           reactimate $ repaint lifePanel <$ (() <$ changes) `union` zooms
           sink pauseButton [text :== (\ t -> if t then "❚❚" else "▶") <$> active]
           sink genLabel [text :== ("generation: " ++) . show . fst <$> life]
           let updateTimer n = set lifeTimer [interval := (100 - n) * 3 + 10]
           reactimate $ (get speedSlider selection >>= updateTimer) <$ slides

  compile network >>= actuate
  where modifyGrid zoom (MouseLeftDown (Point x y) _) = second $ modify (x `div` zoom, y `div` zoom)
        modifyGrid zoom (MouseLeftDrag (Point x y) _) = second $ setPx (x `div` zoom, y `div` zoom) True
        modifyGrid _ _                                = id

renderLife :: Int -> LifeGrid -> DC a -> Rect -> IO ()
renderLife zoom grid ctx _ = sequence_ $ [drawPx x y | x <- [0..width - 1], y <- [0..height - 1], grid ! (Z :. x :. y) == 1]
  where Z :. width :. height = extent grid
        drawPx x y = drawRect ctx (Rect (x * zoom) (y * zoom) zoom zoom) [bgcolor := black]
