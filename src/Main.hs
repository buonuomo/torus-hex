module Main where

import Model
import View
import Controller

import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Interact

main :: IO ()
main = do
    screenSize <- getScreenSize
    interactIO FullScreen blue (initWorld screenSize) draw update control

control :: Controller -> IO ()
control = const (pure ())
