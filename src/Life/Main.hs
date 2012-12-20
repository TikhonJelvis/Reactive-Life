module Main where

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX
import Life.Game

scale :: Int
scale = 4

main :: IO ()
main = start $ do
  mw          <- frame     [text := "John Conway's Game of Life", resizeable := False]
  lifeTimer   <- timer  mw [interval := 50]
  lifePanel   <- panel  mw [bgcolor := white]
  pauseButton <- button mw [text := "▶"]

  set mw [layout := column 2 [minsize (sz 40 25)   $ widget pauseButton,
                              minsize (sz 800 800) $ widget lifePanel]]

  network <- compile $ do
    time   <- event0 lifeTimer   command
    pauses <- event0 pauseButton command
    clicks <- filterJust . fmap toClick <$> event1 lifePanel mouse
    let active  = accumB False $ not <$ pauses
        changes = whenE active (step <$ time) `union`
                  (modify <$> clicks)
        life    = accumB (blank 100 100) changes
    sink lifePanel [on paint :== renderLife <$> life]
    reactimate $ repaint lifePanel <$ changes
    let symb b = if b then "❚❚" else "▶"
    sink pauseButton [text :== symb <$> active]

  actuate network
  where toClick (MouseLeftDown (Point x y) _) = Just (x `div` scale, y `div` scale)
        toClick _                             = Nothing

renderLife :: Life -> DC a -> Rect -> IO ()
renderLife grid ctx _ = mapM_ drawPx $ render grid
  where drawPx (x, y) = drawRect ctx (Rect (x * scale) (y * scale) scale scale) [bgcolor := black]
