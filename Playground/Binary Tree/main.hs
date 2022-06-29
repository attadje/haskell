import Core.DataTypes 
import Core.Logic
import Core.Data







-- bTreeToList :: BTree a -> [a]
-- bTreeToList Empty = []
-- bTreeToList (Node x (L xs) (R xss)) = [x] ++ (bTreeToList xs) ++ (bTreeToList xss)




-- foldrBTree'' :: (Bool -> a -> Bool) -> Bool -> [a] -> Bool
-- foldrBTree'' f acc tree = foldr f acc tree



foldrBTree' :: (t -> t -> t) -> t -> BTree t -> t
foldrBTree' f acc Empty = acc
foldrBTree' f acc (Node x (L xs) (R xss)) = f acc' (foldrBTree' f acc xs)
    where 
        acc' = f x (foldrBTree' f acc xss)

allF' :: Foldable t1 => (t2 -> Bool) -> t1 t2 -> Bool
allF' f xs = foldr (\cv nv -> f cv || nv) False xs


main :: IO ()
main = do 

    putStrLn ("\n" ++ "Search if the number 14 is in the binary tree using the mapBTree function:")
    let isInTree = bSearch 14 binaryTree 
    return isInTree >>= (\bool -> msgBTSearch bool 14)

    putStrLn "Add 1 to all the node using Functor instance:"
    let add1 = fmap (+1) binaryTree
    putStrLn (show add1 ++ "\n")

    putStrLn "Add 1 to all the node using applicative instance:"
    let add1' = pure (+1) <*> binaryTree 
    putStrLn (show add1' ++ "\n")


    print (foldrBTree (+) 0 binaryTree)

 

   


    
   





   











