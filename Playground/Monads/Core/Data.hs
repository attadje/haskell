module Core.Data where
    import Core.DataTypes
    import Core.Logic

    -- Workers informations
    djessy, roberto, nicola   :: Worker
    djessy = createWorker "Djessy""Atta" 10
    roberto = createWorker "Roberto" "" 10 
    nicola = createWorker "Nicola" "Vidon" 10
    
    -- List with the worker informations
    team :: Workers a
    team = Info djessy 
        (Info roberto
        (Info nicola 
        Empty
        ))