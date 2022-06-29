
import Core.Logic
import Core.Data
import Core.DataTypes

main :: IO ()
main = do 
    -- Using basic function
    putStrLn "Add one year to the Roberto and Djessy experience using basic function: \n"
    print (add 1 (yearOfExp djessy))
    print (add 1 (yearOfExp roberto)) -- It should faild because the roberto experience is missing
    putStrLn ""
    
    -- Using functor
    putStrLn "Add one year to the Roberto and Djessy experience using functor: \n"
    print (fmap (+1) (yearOfExp roberto)) -- It should faild because the roberto experience is missing
    print ((+1) <$> (yearOfExp djessy))
    putStrLn ""

    -- Using Applicative
    putStrLn "Add one year to the Roberto and Djessy experience using applicative: \n"
    print (pure (+1) <*> (yearOfExp djessy))
    print ((+) <$> pure 1 <*> (yearOfExp roberto)) -- It should faild because the roberto experience is missing
    putStrLn ""

    -- Using Monad 
    putStrLn "Add one year to the Roberto experience using monad: \n"
    print ((yearOfExp roberto) >>= (\x -> return (x + 1))) -- It should faild because the roberto experience is missing
    print ((yearOfExp djessy) >>= (\x -> return (x + 1)))
    putStrLn ""

    -- Using Monoids
    putStrLn "Calculate the years experiance of the team: \n"
    print ((yearOfExp djessy) <> (yearOfExp nicola) <> (yearOfExp roberto)) -- calculate the team experiences
    putStrLn ""


