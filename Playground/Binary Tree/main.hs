import Core.DataTypes 
import Core.Logic
import Core.Data



main :: IO ()
main = do 

    putStrLn ("\n" ++ "Search if the number 14 is in the binary tree:")
    let isInTree = bSearch 14 binaryTree 
    return isInTree >>= (\bool -> msgBTSearch bool 14)

    putStrLn "Add 1 to all the node using Functor instance:"
    let add1 = fmap (+1) binaryTree
    putStrLn (show add1 ++ "\n")

    putStrLn "Add 1 to all the node using applicative instance:"
    let add1' = pure (+1) <*> binaryTree 
    putStrLn (show add1' ++ "\n")

    putStrLn "Add 1 to all the node using functor and applicative instance:"
    let add1'' = (+) <$> pure 1 <*> binaryTree 
    putStrLn (show add1'' ++ "\n")

    putStrLn "Calculate the sum of all the node:"
    putStrLn ("The sum of all the node is equal to " ++ show (foldrBTree (+) 0 binaryTree) ++ ".\n")


 

   


    
   





   











