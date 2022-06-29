import Data.Either
import Data.List
import Core.GameDataTypes 
import Core.GameView
import Core.GameLogic
  
main :: IO ()
main = do
  putStrLn "##############################"
  putStrLn "######## Tic-Toc-Toe #########"
  putStrLn "##############################"
  putStrLn " "
  putStrLn "Press enter to begin the game:"

  -- Initialisation a 3*3 grid and the game view
  init <- getLine
  let gridState = gState 3
  let gView = gameView 3

  -- Start to ask the name of the players
  putStrLn "Enter the name of the player 1 (X):"
  player1 <- getLine

  putStrLn "\nEnter the name of the player 2 (O):"
  player2 <- getLine

  -- Give info about how to make a move
  putStrLn "\nTo make a move, enter the coordonate of the cell you want to play.\nExemple: (Row number, Column number).\n"
  
  -- Launch the game 
  plays player1 player2 gridState gView


-- This function allow to plays the game until there is a winner or if all the cells are played 
plays :: [Char] -> [Char] -> GridState -> [((Int, Int), String)] -> IO b
plays player1 player2 gridState gView = do
  
  -- Take the play of the player 1
  putStrLn (player1 ++ " plays (X):")
  showGameView gView 
  playsP1 <- readLn

  let gridState1 = makeAMove (move playsP1 X) gridState
  let state1 = checkGameState X gridState1
  let updateView1 = updateGameView playsP1 X gView

  checkState player1 player2 state1

  -- Take the play of the player 2
  putStrLn (player2 ++ " plays (O):")
  showGameView updateView1
  playsP2 <- readLn

  let gridState2 = makeAMove (move playsP2 O) gridState1
  let state2 = checkGameState O gridState2
  let updateView2 = updateGameView playsP2 O updateView1

  checkState player1 player2 state2
  
  plays player1 player2 gridState2 updateView2


-- This function allow to make an action depending of the game state.
checkState :: [Char] -> [Char] -> GameState -> IO ()
checkState player1 player2 state 
        | state == Running = putStrLn ""
        | state == TieGame = print "Tie Game" 
        | state == Xwins = do 
                putStrLn (player1 ++ " Win \n") 
                main
        | state == Owins = do 
                putStrLn (player2 ++ " Win \n")
                main






