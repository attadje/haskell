
import Core.DataTypes
import Core.Data
import Core.Logic




main = do
    putStrLn "\nShow all the available books at the Manzarine library:\n"
    getBooks manzarine >>= showBooksTitles 

    putStrLn "\nSearch book by author in all Paris libraries:\n"
    putStrLn "c"
    author <- getLine
    putStrLn "\nBooks available:"
    getBooks (filterByAuth author (manzarine <> centrePompidou)) >>= showBooksTitles

 
   