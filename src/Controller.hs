{-# LANGUAGE RecordWildCards #-}

module Controller where

import Constants 
import Model

import Data.Array
import Data.Bifunctor
import Data.Biapplicative
import Data.List (minimumBy)
import Data.Ord (comparing)
import Graphics.Gloss.Interface.IO.Interact
import System.Exit

-- Should I use lens for update?
-- pros: easier getting/setting, less pattern matching
-- cons: it's probably overkill/overengineering

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
          then
            case toHex w location of
              Nothing -> pure $ w { mouseDown = Nothing }
              Just coords -> pure $ placeTile coords w
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

-- w { board = board , mouseDown = Nothing } -- TODO
-- | converts given screen coords to board coordinates and then modifies state
-- accordingly
placeTile :: (Int, Int) -> World -> World
placeTile coord w@World{..} = w
    { --board = put coord currentPlayer board
    board = put coord currentPlayer board
    , currentPlayer = nextPlayer currentPlayer
    , mouseDown = Nothing
    , debug = show board
    }
    where
        -- | put a tile of the given color at the given coords
        put :: (Int, Int) -> Player -> Board -> Board
        put coords player board = 
            array ((0,0), (10,10)) 
              [ ((i,j), val) | i <- [0..10]
                             , j <- [0..10]
                             , let val = if (i,j) == coords then playerColor player else board!(i,j)
                             ]

-- | convert screen coords to hex coords
toHex :: World -> (Float, Float) -> Maybe (Int, Int)
toHex World{..} loc = hexRound
                    . deshearTile 
                    . bimap negate negate 
                    . biliftA2 (-) (-) coords 
                    . biliftA2 (/) (/) loc 
                    $ scaling

deshearTile :: (Float, Float) -> (Float, Float)
deshearTile (x,y) = ((x - b * y / c)/a, y/c)
  where
    a = rad * sqrt 3 + 1
    b = a / 2
    c = 15.866

shearTile :: (Float, Float) -> (Float, Float)
shearTile (x, y) = (a*x + b*y, c*y)
  where
    a = rad * sqrt 3 + 1
    b = a / 2
    c = 15.866

-- | given a fractional hex coord, turn into integer
hexRound :: (Float, Float) -> Maybe (Int, Int)
hexRound (x,y) = let options = [ (floor x, floor y)
                               , (floor x, ceiling y)
                               , (ceiling x, floor y)
                               , (ceiling x, ceiling y)
                               ]
                     floptions = map (shearTile . bimap fromIntegral fromIntegral) options
                     point = shearTile (x, y)
                     closest =  minimumBy (comparing (euclidDist point)) floptions
                  in
                    if hexDist closest point <= rad then Just (bimap round round (deshearTile closest)) else Nothing
   where
      euclidDist :: (Floating b) => (b, b) -> (b, b) -> b
      euclidDist (a, b) (x, y) = sqrt ((a - x)^2 + (b - y)^2)

      hexDist :: (Ord b, Floating b) => (b, b) -> (b, b) -> b
      hexDist (a, b) (x, y) =
          let f θ r = 
                  if 0 <= θ  && θ <= pi / 3 
                     then  r * sin (2/3 * pi - θ) / sin (pi / 3)
                     else  f (θ - pi / 3) r
           in f radius angle
          where radius = euclidDist (a, b) (x, y)
                angle = atan ((x - a) / (y - b))

normalize :: (Float, Float) -> (Float, Float)
normalize (x, y)
  | x > boardWidth = normalize (x - boardWidth, y)
  | x < -boardWidth = normalize (-x - boardWidth, y)
  | y > boardHeight = normalize (x + boardSkew , y - boardHeight)
  | y < -boardHeight = normalize (x + boardSkew, -y + boardHeight)
  | otherwise = (x, y)
  where
      boardWidth = 11 * (2 * rad * cos (pi / 6) + spc)
      -- TODO: maybe tweak a bit
      -- boardHeight = 11 * (1.5 * rad + spc)
      boardHeight = 11 * 15.866
      -- This might also just be boardWidth / 2
      boardSkew = 11 * (rad * cos (pi / 6) + spc / 2)
