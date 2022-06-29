module Core.GameDataTypes 
( CellState (..)
, Cell (..)
, GridState (..)
, GameState (..)
, Player (..)
, GridSize (..)
) where

data CellState
  = EmptyCell
  | X
  | O
  deriving (Show, Eq)

data Cell
  = Cell
      Int
      Int
      CellState
  deriving (Show)

type GridState = [Cell]

data GameState
  = Running
  | Xwins
  | Owins
  | TieGame
  deriving (Show, Eq)

data Player
  = PlayerX
  | PlayerO
  deriving (Show)

data GridSize = GridSize
  { row :: Int,
    col :: Int
  }
  deriving (Show)