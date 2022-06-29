module Core.Logic where
    import Core.DataTypes
    import Core.Data
    import Data.Either

    bookFilter :: (a -> Bool) -> Books a -> Books a
    bookFilter f (Book x NoBook) = if f x then Book x NoBook else NoBook
    bookFilter f (Book x xs) 
        | f x = Book x $ bookFilter f xs
        | otherwise = bookFilter f xs 

    -- This function allow to filter a book list by author
 
   


    filterByAuth :: String -> Books BookInfo -> Either String (Books BookInfo)
    filterByAuth auth books = case books of
        NoBook -> Left "Error"
        _      -> Right avBooks
        where
        avBooks = bookFilter (\x -> if author x == auth then True else False) books

    -- Display all the book titles in the list of books
    
    
    showBooksTitles :: Either String (Books BookInfo) -> IO ()
    showBooksTitles e = case e of
        Left "Error" -> putStrLn "No book found for this author"
        Right (Book x xs) -> show (Book x xs)
        where 
            show (Book x xs) 
                | xs /= NoBook = do 
                    putStrLn (title x)
                    show xs
        
                | xs == NoBook = putStrLn ((title x) ++ "\n")












 
   


   



    

    