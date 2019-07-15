{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Arrow ((&&&))
import Data.Complex
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Interact
import System.Exit

(<>) = mappend

main :: IO ()
main = do
    screenSize <- getScreenSize
    interactIO FullScreen blue (initWorld screenSize) draw update control

draw :: World -> IO Picture
draw (World {..}) = pure . uncurry scale scaling . uncurry translate coords $ tiling

update :: Event -> World -> IO World
--update (EventResize (dx, dy)) (World (x, y)) = pure (World ((x + fromIntegral dx), (y + fromIntegral dy)))
update (EventKey (SpecialKey KeyLeft) Down _ _) w@(World { coords = (x, y), scaling = (sx, sy)}) = pure (w { coords = normalize (x + 10 / sx, y)})
update (EventKey (SpecialKey KeyRight) Down _ _) w@(World { coords = (x, y), scaling = (sx, sy)}) = pure (w { coords = normalize (x - 10 / sx, y)})
update (EventKey (SpecialKey KeyDown) Down _ _) w@(World { coords = (x, y), scaling = (sx, sy)}) = pure (w { coords = normalize (x, y + 10 / sy)})
update (EventKey (SpecialKey KeyUp) Down _ _) w@(World { coords = (x, y), scaling = (sx, sy)}) = pure (w { coords = normalize (x, y - 10 / sy)})
update (EventKey (MouseButton LeftButton) Down _ location) w = pure $ w { mouseDown = Just location, dragPos = location }
update (EventKey (MouseButton LeftButton) Up _ location) w@(World{coords = (x,y),..}) =
   case mouseDown of
     Nothing -> pure w
     Just down ->
       if down == location
          then pure $ w { coords = (x + 10, y + 10), mouseDown = Nothing }
          else pure $ w { mouseDown = Nothing }
update (EventKey (MouseButton WheelDown) Down _ _) w@(World { scaling = (x, y)}) = pure $ w { scaling = (x + 0.2, y + 0.2) }
update (EventKey (MouseButton WheelUp) Down _ _) w@(World { scaling = (x, y)}) = pure $ w { scaling = (max 1 (x - 0.2), max 1 (y - 0.2)) }
update (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
update (EventMotion (dx, dy)) w@(World {coords = (x,y),..}) =
    case mouseDown of
      Nothing -> pure w
      Just (downx, downy) ->
        let (dpx, dpy) = dragPos
            (sx, sy) = scaling
         in pure $ w { coords = normalize (x - (dpx - dx)/(2 * sx), y - (dpy - dy)/(2 * sy)), dragPos = (dx, dy) }
update _ w = pure w

normalize :: (Float, Float) -> (Float, Float)
normalize (x, y)
  | x > boardWidth = normalize (x - boardWidth, y)
  | x < -boardWidth = normalize (-x - boardWidth, y)
  | y > boardHeight = normalize (x, y - boardHeight)
  | y < -boardHeight = normalize (x, -y + boardHeight)
  | otherwise = (x, y)

control :: Controller -> IO ()
control = const (pure ())


row :: Picture
row = pictures $ map (\(p,n) -> translate (n*18.3205) 0 p) $ zip (repeat (hexagon 10)) [0..10]

board :: Picture
board = pictures $ map (\(p,n) -> translate (n * 9.1603) (n * 15.866) p) $ zip (repeat row) [0..10]

rowWithSpace :: Float -> Picture
rowWithSpace s = pictures $ map (\(p,n) -> translate (n * (2 * 10 * cos (pi / 6) + s)) 0 p) $ zip (repeat (hexagon 10)) [0..10]

boardWithSpace :: Float -> Picture
boardWithSpace s = pictures $ map (\(p,n) -> translate (n * a * cos (pi / 3)) (n * a * sin (pi / 3)) p) $ zip (repeat (rowWithSpace s)) [0..10]
  where a = 2 * 10 * cos (pi / 6) + s

borderBoard :: Picture
borderBoard =  color black (translate (-10 * cos (pi / 6)) (-10 * sin (pi / 6))  $ polygon [(0,0),(10 * 18.3205 + d, 0), (10*18.3205 + 10 * 9.1603 + d + 10 / sqrt 3, 10 * 15.866 + 10), (10 * 9.1603 + 10 / sqrt 3, 10*15.866 + 10)]) <> color white (boardWithSpace 1)
  where c = 20 * cos (pi / 6)
        d = 5 / sqrt 3 + 5 * sqrt 3

lattice :: [(Float, Float)]
lattice = (,) <$> [-6..5] <*> [-4..3]

boardWidth :: Float
boardWidth = 11 * 18.3205

-- TODO: This is wrong
boardHeight :: Float
boardHeight = 11 * 15.866 + 5.866

skew :: (Float, Float) -> (Float, Float)
skew (x,y) = (c*x + a*y, b*y)
  where
    c = 11 * 18.3205
    b = 11 * 15.866
    a = 11 * 9.1603

tiling :: Picture
tiling = 
    let latticeSkew = map skew lattice
     in pictures $ map (\(x,y) -> translate x y borderBoard) latticeSkew

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
    , scaling = (2,2)
    , coords = (0,0) 
    , mouseDown = Nothing
    , dragPos = (0,0)
    }

data World = World 
    { coords :: (Float, Float) 
    , scaling :: (Float, Float)
    , screen :: (Int, Int)
    , mouseDown :: Maybe (Float, Float)
    , dragPos :: (Float, Float)
    } deriving (Show, Eq)
