module Core.GameView 
( gameView
, updateGameView
, showGameView 
) where

import Core.GameDataTypes

{- 
This function allow to initialze the data for grid view.
The function takes the number of col ou row and associate each coordonate of the grid with a string
who represent the state of the cell ("X" | "O" or empty "*"")
-}
gameView :: (Num b, Enum b) => b -> [((b, b), String)]
gameView nbCol = [((x,y), "*") | x <- [1..nbCol], y <- [1..nbCol]]

{- 
This function allow to update the data of the grid view.
The funtion takes the coordonate, the player pawn and the grid view data.
Then the find the coordonate in the grid view data and replace the string state by the player pawn
-} 
updateGameView :: (Eq a, Eq b) => (a, b) -> CellState -> [((a, b), String)] -> [((a, b), String)]
updateGameView (x,y) pawn gView = go
        where
          pawnToStr = if pawn == X then "X" else "O"
          go = [if c == (x,y) 
                then ((x,y), pawnToStr) 
                else (c,p) | (c,p) <- gView] 

{-
This function allow to show the grid on the console.
The function take the grid view data and browse each element of the list.
When the value of the column (y) is equal to the number of colonne we add a space at the begenin then the string pawn and the back line (\d).
When the value of the column (y) is equal to one we add the number of row at the begenin then we add a space and the string pawn.
If the condition belown is not find we just add a space folowing by the string pawn.
 -}
showGameView :: (Eq a, Num a, Show a) => [((a, Int), [Char])] -> IO ()
showGameView gView = do 
        let nbCol = length [e|e@((x,_),_) <- gView, x == 1]
        let view = [if y == nbCol 
                   then (" " ++ pawn ++  "\n") 
                   else if y == 1 
                        then (show x ++ " " ++ pawn) 
                        else (" " ++ pawn) | ((x,y),pawn) <- gView]
                        
        putStrLn " "
        putStrLn $ " " ++ concat [" " ++ show x | x <- [1..nbCol]]
        putStrLn $ concat view

