
import Core.DataTypes
import Core.Data
import Core.Logic




main = do
    putStrLn "\nShow all the available books in the Manzarine library:\n"
    return manzarine >>= showBooksTitles 

    putStrLn "\nSearch book from a specific author in all Paris libraries."
    putStrLn "Enter the book author (LastName, FirstName):"

    author <- getLine
    putStrLn "\nBooks available:"
    return (filterByAuth author (manzarine <> centrePompidou)) >>= showBooksTitles

 
   