{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Arrow ((&&&))
import Data.Complex
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Interact
import System.Exit

main :: IO ()
main = do
    screenSize <- getScreenSize
    interactIO FullScreen white (initWorld screenSize) draw update control

draw :: World -> IO Picture
draw (World {..}) = pure (uncurry translate coords $ tile)

update :: Event -> World -> IO World
--update (EventResize (dx, dy)) (World (x, y)) = pure (World ((x + fromIntegral dx), (y + fromIntegral dy)))
update (EventKey (SpecialKey KeyLeft) Down _ _) w@(World { coords = (x, y)}) = pure (w { coords = (x + 10, y)})
update (EventKey (SpecialKey KeyRight) Down _ _) w@(World { coords = (x, y)}) = pure (w { coords = (x - 10, y)})
update (EventKey (SpecialKey KeyDown) Down _ _) w@(World { coords = (x, y)}) = pure (w { coords = (x, y + 10)})
update (EventKey (SpecialKey KeyUp) Down _ _) w@(World { coords = (x, y)}) = pure (w { coords = (x, y - 10)})
update (EventKey (MouseButton LeftButton) Down _ location) w = pure $ w { mouseDown = Just location, dragPos = location }
update (EventKey (MouseButton LeftButton) Up _ location) w@(World{coords = (x,y),..}) =
   case mouseDown of
     Nothing -> pure w
     Just down ->
       if down == location
          then pure $ w { coords = (x + 10, y + 10), mouseDown = Nothing }
          else pure $ w { mouseDown = Nothing }
update (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
update (EventMotion (dx, dy)) w@(World {coords = (x,y),..}) =
    case mouseDown of
      Nothing -> pure w
      Just (downx, downy) ->
        let (dpx, dpy) = dragPos in
            pure $ w { coords = (x - (dpx - dx)/2, y - (dpy - dy)/2), dragPos = (dx, dy) }
update _ w = pure w

control :: Controller -> IO ()
control = const (pure ())

-- | tile hexagons
tile :: Picture
tile = pictures $ map (\(p,n) -> translate (n*21) 0 p) $ zip (repeat (hexagon 10)) [0..100]

-- | draw a regular hexagon with the given lenght from the center to a vertex
hexagon :: Complex Float -> Picture
hexagon r = rotate 90 
          . polygon 
          . take 6 
          . map ((realPart &&& imagPart) . (* r) . (\k -> exp (0 :+ 2 * pi * k / 6))) 
          $ [0..]

initWorld :: (Int, Int) -> World
initWorld screen = World 
    { screen = screen
    , coords = (0,0) 
    , mouseDown = Nothing
    , dragPos = (0,0)
    }

data World = World 
    { coords :: (Float, Float) 
    , screen :: (Int, Int)
    , mouseDown :: Maybe (Float, Float)
    , dragPos :: (Float, Float)
    } deriving (Show, Eq)