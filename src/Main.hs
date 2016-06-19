module Main where

import           Control.Monad               (void)

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = void $ do
  return w # set title "FRP Demo"
  getBody w #+ [ UI.h1 #+ [string "Hello World"]
               , UI.div #+ [string "Here is some text. Making HTML like this is actually reasonable!"]
               ]
