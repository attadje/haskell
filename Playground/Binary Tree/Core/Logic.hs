module Core.Logic where
    import Core.DataTypes
    
    -- This function search if the node is in the binary tree.
    bSearch :: Ord a => a -> BTree a -> Bool
    bSearch key (Node x (L Empty) (R Empty)) = if key == x then True else False
    bSearch key (Node x (L xs) (R xss)) 
        | key == x = True
        | key < x = bSearch key xs 
        | key > x = bSearch key xss

    -- This function take the result from the bSearch function and print a message in function of the result.
    msgBTSearch :: Show a => Bool -> a -> IO ()
    msgBTSearch b a 
        | b         = putStrLn (show a ++ " have been found in the binary tree.\n")
        | otherwise = putStrLn (show a ++ " haven't be found in the binary tree\n") 


    -- Using map function on the binary tree structure.
    mapBTree :: (a -> a) -> BTree a -> BTree a
    mapBTree f (Node x Empty Empty) = Node (f x) (L Empty) (R Empty)
    mapBTree f (Node x Empty (R xss)) = Node (f x) (L Empty) (R $ fmap f xss)
    mapBTree f (Node x (L xs) Empty) = Node (f x) (L $ fmap f xs) (R Empty)
    mapBTree f (Node x (L xs) (R xss)) = Node (f x) (L $ fmap f xs) (R $ fmap f xss)

    -- Using foldr on the binary tree structure.
    foldrBTree :: (t -> t -> t) -> t -> BTree t -> t
    foldrBTree f acc Empty = acc
    foldrBTree f acc (Node x (L xs) (R xss)) = f acc' (foldrBTree f acc xs)
        where 
            acc' = f x (foldrBTree f acc xss)

  
    -- An other approch 

     

      


