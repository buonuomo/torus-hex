{-# LANGUAGE RecordWildCards #-}

module View where

import Constants
import Model

import Control.Arrow ((&&&))
import Data.Array
import Data.Bifunctor
import Data.Complex
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

(<>) = mappend

draw :: World -> IO Picture
draw (World {..}) = pure $ (uncurry scale scaling . uncurry translate coords $ tiling board)

shearTile :: (Float, Float) -> (Float, Float)
shearTile (x, y) = (a*x + b*y, c*y)
  where
    a = rad * sqrt 3 + 1
    b = a / 2
    c = 15.866

-- | draw a board at each point in the lattice
tiling :: Board -> Picture
tiling board = pictures 
             . map (\(x,y) -> translate x y (boardWithTiles rad spc board)) 
             . map shearBoard
             $ lattice

boardWithTiles :: Float -> Float -> Board -> Picture
boardWithTiles r s board = borderBoard r s <> tiles
  where
      tiles = pictures 
            . map (\(i,e) -> drawTile (shearTile (bimap fromIntegral fromIntegral i)) e)
            . assocs 
            $ board

drawTile :: (Float, Float) -> Tile -> Picture
drawTile _ Empty = blank
drawTile coords Black = color black (uncurry translate coords (circle (rad / 2)))
drawTile coords White = color white (uncurry translate coords (circle (rad / 2)))

shearBoard :: (Float, Float) -> (Float, Float)
shearBoard (x,y) = (a*x + b*y, c*y)
  where
      a = 11 * (2 * rad * cos (pi / 6) + spc)
      c = 11 * 15.866 -- TODO
      --c = 11 * (1.5 * rad + spc)
      b = 11 * (rad * cos (pi / 6) + spc / 2)

-- | square unit lattice corresponding to the boards we need to show
lattice :: [(Float, Float)]
--lattice = (,) <$> [-8..8] <*> [-4..3]
lattice = (,) <$> [-6..5] <*> [-4..3]

-- | board with the border
borderBoard :: Float -> Float -> Picture
borderBoard r s = 
    color black 
      (translate (-r * cos (pi / 6)) (-r * sin (pi / 6)) 
        (background r s)) <>
    color orange
      (boardWithSpace r s)

-- | a rhombus that goes behind the board to serve as the background
background :: Float -> Float -> Picture
background r s = polygon 
               [ (0,0)
               , (10 * w + d, 0)
               , (10 * 1.5 * w  + d + r / sqrt 3, 10 * 15.866 + r) -- TODO: figure out where i got this number
               , (10 * 0.5 * w + r / sqrt 3, 10 * 15.866 + r)
               ]
  where w = 2 * r * cos (pi / 6) + s
        d = 5 / sqrt 3 + 5 * sqrt 3

-- | board with space in between
boardWithSpace :: Float -> Float -> Picture
boardWithSpace r s = pictures 
                   . map (\(p,n) -> translate (n * a * cos (pi / 3)) (n * a * sin (pi / 3)) p) 
                   . zip (repeat (rowWithSpace r s)) 
                   $ [0..10]
  where a = 2 * r * cos (pi / 6) + s

-- | several hexagons of given radius in a row with space in between
rowWithSpace :: Float -> Float -> Picture
rowWithSpace r s = pictures 
                 . map (\(p,n) -> translate (n * (2 * r * cos (pi / 6) + s)) 0 p) 
                 . zip (repeat (hexagon (r :+ 0))) 
                 $ [0..10]

-- | draw a regular hexagon with the given length from the center to a vertex
hexagon :: Complex Float -> Picture
hexagon r = rotate 90 
          . polygon 
          . take 6 
          . map ((realPart &&& imagPart) . (* r) . (\k -> exp (0 :+ 2 * pi * k / 6))) 
          $ [0..]

{-
row :: Picture
row = pictures $ map (\(p,n) -> translate (n*18.3205) 0 p) $ zip (repeat (hexagon 10)) [0..10]

board :: Picture
board = pictures $ map (\(p,n) -> translate (n * 9.1603) (n * 15.866) p) $ zip (repeat row) [0..10]
-}
