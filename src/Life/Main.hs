{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Array.Repa     ((:.) (..), Z (..))
import qualified Data.Array.Repa     as R
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

  tt  <- timer mw [interval := 100]

  let network = do
        time   <- event0 tt command
        clicks <- event0 pauseButton command
        let start  = rPentonimo `embed` blank 200 200
            active = accumB True $ not <$ clicks
            life   = accumB start . whenE active $ step <$ time
        sink lifePanel [on paint :== renderLife <$> life]
        reactimate $ repaint lifePanel <$ time
        reactimate $ (\ t -> set pauseButton [text := if t then "▶" else "❚❚"]) <$> active <@ clicks

  compile network >>= actuate

renderLife :: LifeGrid -> DC a -> Rect -> IO ()
renderLife grid ctx _ = V.forM_ (V.imap toPx $ R.toUnboxed grid) $ \ (x, y, p) ->
  when p $ drawRect ctx (Rect (x * 4) (y * 4) 4 4) [bgcolor := black]
  where (Z :. width :. height) = R.extent grid
        toPx i v = (i `rem` width, i `div` height, v == 1)
