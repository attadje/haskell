
module Core.Data where
    import Core.DataTypes
    
    -- Books
    bookOne = BookInfo 
        { title = "Blockchain",
          author = "Leloup, Laurant",
          numberOfPage = 70,
          location = "L3"}

    bookTwo = BookInfo
         { title = "The Age Of Data",
           author = "Niggli",
           numberOfPage = 400,
           location = "N1"}

    bookThree = BookInfo
     { title = "A Brief History of Time",
       author = "Hawking, Stephan",
       numberOfPage = 272,
       location = "H3"}

    bookFour = BookInfo
       { title = "The Theory Of Everything",
         author = "Hawking, Stephan",
         numberOfPage = 272,
         location = "H3"}

    bookFive = BookInfo
        { title = "Fondation, Book 1",
          author = "Asimov, Isaac",
          numberOfPage = 350,
          location = "A5"}

    -- Paris library
    manzarine        = Book bookOne $ Book bookTwo $ Book bookThree NoBook 
    centrePompidou   = Book bookFour $ Book bookFive NoBook 

    library   = Book bookFour NoBook

    
 
   




    