{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Array.Repa     ((:.) (..), Z (..), (!), extent)
import qualified Data.Vector.Unboxed as V

import           Graphics.UI.WX      hiding (Event)

import           Reactive.Banana
import           Reactive.Banana.WX

import           Life.Game

main :: IO ()
main = start $ do
  mw          <- frame     [text := "John Conway's Game of Life", resizeable := False]
  lifePanel   <- panel  mw [bgcolor := white]
  pauseButton <- button mw [text := "❚❚"]

  set mw [layout := column 2 [minsize (sz 50 25)   $ widget pauseButton,
                              minsize (sz 800 800) $ widget lifePanel]]

  tt <- timer mw [interval := 100]

  let network = do
        time   <- event0 tt command
        pauses <- event0 pauseButton command
        clicks <- event1 lifePanel mouse
        let start  = rPentonimo `embed` blank 200 200
            active = accumB True $ not <$ pauses
            life   = accumB start $ whenE active (step <$ time) `union` (modifyGrid <$> clicks)
        sink lifePanel [on paint :== renderLife <$> life]
        reactimate $ repaint lifePanel <$ time
        reactimate $ (\ t -> set pauseButton [text := if t then "▶" else "❚❚"]) <$> active <@ pauses

  compile network >>= actuate
  where modifyGrid (MouseLeftDown (Point x y) _) = modify (x `div` 4, y `div` 4)
        modifyGrid _                             = id

renderLife :: LifeGrid -> DC a -> Rect -> IO ()
renderLife grid ctx _ = sequence_ $ [drawPx x y | x <- [0..width - 1], y <- [0..height - 1], grid ! (Z :. x :. y) == 1]
  where Z :. width :. height = extent grid
        drawPx x y = drawRect ctx (Rect (x * 4) (y * 4) 4 4) [bgcolor := black]
