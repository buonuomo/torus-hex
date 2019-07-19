module Model where

import Data.Array

data Tile = Empty 
          | White 
          | Black
          deriving (Show, Eq)

data Player = PlayerWhite
            | PlayerBlack
            deriving (Show, Eq)

-- | advance to the next player
nextPlayer :: Player -> Player
nextPlayer PlayerWhite = PlayerBlack
nextPlayer PlayerBlack = PlayerWhite

playerColor :: Player -> Tile
playerColor PlayerWhite = White
playerColor PlayerBlack = Black

type Board = Array (Int, Int) Tile

initWorld :: (Int, Int) -> World
initWorld screen = World 
    { screen = screen
    , scaling = (2,2)
    , coords = (0,0) 
    , mouseDown = Nothing
    , dragPos = (0,0)
    , currentPlayer = PlayerWhite
    , board = array ((0, 0), (10, 10)) [((i, j), Empty) | i <- [0..10], j <- [0..10]]
    , debug = ""
    }

data World = World 
    { coords :: (Float, Float) 
    , scaling :: (Float, Float)
    , screen :: (Int, Int)
    , mouseDown :: Maybe (Float, Float)
    , dragPos :: (Float, Float)
    , currentPlayer :: Player
    , board :: Board
    , debug :: String
    } deriving (Show, Eq)
