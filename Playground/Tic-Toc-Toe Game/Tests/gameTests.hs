import GameDataTypes

-- Grid size
gridSize :: GridSize
gridSize = GridSize 3 3

-- Grid Initialisation
emptyGState :: GridState
emptyGState = gState $ row gridSize

hWin :: CellState -> [Cell]
hWin player = map (\l@(Cell x y s) -> if x == 1 then (Cell x y player) else l) emptyGState

vWin :: CellState -> [Cell]
vWin player = map (\l@(Cell x y s) -> if y == 1 then (Cell x y player) else l) emptyGState

diagWin :: CellState -> [Cell]
diagWin player = map (\l@(Cell x y s) -> if x == y then (Cell x y player) else l) emptyGState

draw :: [Cell]
draw = map (\l@(Cell x y s) -> (Cell x y X)) emptyGState