module Core.Logic where
    import Core.DataTypes
    
    setFirstName :: String -> Worker -> Worker
    setFirstName newFName worker
        | newFName == "" =  worker {firstName=Nothing'} 
        | otherwise      = worker {firstName=(pure newFName)}

    setLastName :: String -> Worker -> Worker
    setLastName newLName worker
        | newLName == "" =  worker {firstName=Nothing'} 
        | otherwise      = worker {lastName=(pure newLName)}

    setYearOfExp :: Integer -> Worker -> Worker
    setYearOfExp newYOExp worker = worker {yearOfExp=(pure newYOExp)}


    createWorker :: String -> String -> Integer -> Worker
    createWorker fName lName yExp = w
        where
            w' = Worker (Just' "") (Just' "") (Just' 0)
            w'' = setFirstName fName w'  
            w''' = setLastName lName w''
            w = setYearOfExp yExp w'''
