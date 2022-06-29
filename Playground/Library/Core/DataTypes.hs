
module Core.DataTypes (
    Books (..),
    Title,
    BookInfo (..),
    ) where

type Title = String

data BookInfo = BookInfo {
    title :: String,
    author :: String,
    numberOfPage :: Int,
    location :: String
    } deriving (Show, Eq)
    
data Books a 
    = Book a (Books a) 
    | NoBook   
    deriving (Show, Eq) 
  
instance Semigroup (Books a) where
    Book x NoBook <> Book y ys = Book x (Book y ys)
    Book x xs <> Book y ys = Book x (xs <> Book y ys)

instance Functor Books where
    fmap f (Book x xs) = Book (f x) (fmap f xs)
    fmap f NoBook = NoBook

instance Applicative Books where
    pure bookInfo = Book bookInfo NoBook
    Book f NoBook <*> xs = fmap f xs 








  
  





