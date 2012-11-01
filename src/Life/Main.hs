{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Array.Repa     ((:.) (..), Z (..))
import qualified Data.Array.Repa     as R
import qualified Data.Vector.Unboxed as V

import           Graphics.UI.WX      hiding (Event)
import qualified Graphics.UI.WXCore  as WX

import           Reactive.Banana
import           Reactive.Banana.WX

import           Life.Game

main :: IO ()
main = start $ do
  mw <- frame [text := "Title", resizeable := False]
  lifePanel <- panel mw [bgcolor := white]
  set mw [layout := minsize (sz 800 800) $ widget lifePanel]

  tt  <- timer mw [interval := 100]

  let network :: forall t. NetworkDescription t ()
      network = do time <- event0 tt command
                   let life = accumB (rPentonimo `embed` blank 200 200) $ step <$ time
                   sink lifePanel [on paint :== renderLife <$> life]
                   reactimate $ repaint lifePanel <$ time

  compile network >>= actuate

renderLife :: LifeGrid -> DC a -> Rect -> IO ()
renderLife grid ctx _ = V.forM_ (V.imap toPx $ R.toUnboxed grid) $ \ (x, y, p) ->
  when p $ drawRect ctx (Rect (x * 4) (y * 4) 4 4) [bgcolor := black]
  where (Z :. width :. height) = R.extent grid
        toPx i v = (i `rem` width, i `div` height, v == 1)
