module Core.GameLogic 
( gState
, isValidMove
, makeAMove
, getCStates
, checkHVWin
, checkDiagWin
, checkGameState
, move
) where

import Core.GameDataTypes
import Core.GameView

-- This function allow to create an empty grid with n*n dimension
gState :: Int -> GridState
gState gSize = [Cell x y EmptyCell | x <- [1 .. gSize], y <- [1 .. gSize]]

{-
This function allow to validate a move made by a player.
A move is valid if:
         1) The cell to change is empty.
         2) The cell exist

The aproch is to remove the cells with an empty state and look if the cell to change is in the list.
If the cell is not in the list then it's mean that the cell dosen't exist or it has already been played.
-}
isValidMove :: Cell -> [Cell] -> Bool
isValidMove (Cell x y s) gState = go move availableCells
  where
    move = Cell x y EmptyCell
    availableCells = [(Cell x y s) | (Cell x y s) <- gState, s == EmptyCell]

    go :: Cell -> GridState -> Bool
    go move availableCells
      | move `elem` availableCells = True
      | otherwise = False

{-
This function allow to update a grid state with the move of the player.
The function check if the move is valid. if the move is valid the grid is updated.
If the move is not valid an error is generate.
-}
makeAMove :: Cell -> GridState -> GridState
makeAMove move gState
  | isValidMove move gState = map (\cell -> if cell == move then move else cell) gState
  | otherwise = error "Your move is invalid" -- This part will be improve later with Maybe or Either


-- Method to compare the equality of the position betwen two cell
instance Eq Cell where
  (Cell x1 y1 s1) == (Cell x2 y2 s2) = x1 == x2 && y1 == y2


-- This function allow to get the states from a list off cells
getCStates :: [Cell] -> [CellState]
getCStates cells = map (\(Cell x y s) -> s) cells


-- This function allow to get the size of a grid.
getGSize :: GridState -> Int
getGSize = length . filter (\(Cell x y s) -> x == 1)


{-
This function check if there is a horizontal or a vertical win for any grid size.
The approch is to browse each row of the grid state to find a win combination.
A win combination is the sequence of X or O where the number of sequence is equal to the number of col or row.
-}
checkHVWin :: Int -> CellState -> GridState -> Bool
checkHVWin 0 _ _ = False
checkHVWin gSize pawn gSate
  | sRow == gameWiner = True
  | sCol == gameWiner = True
  | otherwise = checkHVWin (gSize -1) pawn gSate
  where
    gameWiner = replicate (getGSize gSate) pawn
    sRow = getCStates $ filter (\(Cell x y s) -> x == gSize) gSate
    sCol = getCStates $ filter (\(Cell x y s) -> y == gSize) gSate

{-
This function check if there is a diagonal win for any grid size.
For the diagonal left win we just take the state of all the cells when x is equal to y and look if is equal to a sequence of the player pawn(X or O).
For the right win we start with the cell to the position x=1 and y= grid size so for a 3*3 grid we start at the cell (1,3).
and we incrase the x by one an decrase the y by one until x = 0 and look if is equal to a sequence of the player pawn (X or O).
-}
checkDiagWin :: CellState -> GridState -> Bool
checkDiagWin pawn gridState
  | sDiagL == gameWiner = True
  | sDiagR == gameWiner = True
  | otherwise = False
  where
    -- This function allow to get a specific cell from a grid state
    getGridCell :: Int -> Int -> GridState -> GridState
    getGridCell x y = filter (\(Cell x' y' s) -> x' == x && y' == y)

    -- This function allow to get the diagonal right cells from any grid size
    getDiagRCell :: Int -> Int -> GridState -> [Cell]
    getDiagRCell _ 0 _ = []
    getDiagRCell x y gridState = getGridCell x y gridState ++ getDiagRCell (x + 1) (y -1) gridState

    -- This function allow to get the diagonal left cells from any grid size
    getDiagLCell :: GridState -> [Cell]
    getDiagLCell = filter (\(Cell x y s) -> x == y)

    gameWiner = replicate (getGSize gridState) pawn
    sDiagR = getCStates $ getDiagRCell 1 (getGSize gridState) gridState
    sDiagL = getCStates $ getDiagLCell gridState

-- This function determine the state of the game (Xwins | Owins | Running | TieGame)
checkGameState :: CellState -> GridState -> GameState
checkGameState pawn gridState
  | hvWin || dWin = winer
  | nbOfMove < (gSize * gSize) = Running
  | otherwise = TieGame
  where
    gSize = getGSize gridState
    winer = if pawn == X then Xwins else Owins
    hvWin = checkHVWin gSize pawn gridState
    dWin = checkDiagWin pawn gridState
    -- Number of move is equal to the number of non empty cell
    nbOfMove = length $ filter (\(Cell x y s) -> s /= EmptyCell) gridState

-- This function allow to create a cell 
move :: (Int, Int) -> CellState -> Cell
move (x, y) s = Cell x y s







