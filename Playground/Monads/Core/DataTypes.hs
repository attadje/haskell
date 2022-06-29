module Core.DataTypes where

data Maybe' a = 
    Just' a 
    | Nothing' 
    deriving (Show, Eq)

data Worker = Worker 
    { firstName :: Maybe' String,
      lastName :: Maybe' String,
      yearOfExp :: Maybe' Integer 
    } 
    deriving Show

data Workers a = Info Worker (Workers Worker) | Empty

-- Add a wrapped value to another using a basic function
add :: Integer -> (Maybe' Integer) -> Maybe' Integer
add nb Nothing' = Nothing'
add nb (Just' y) = Just' (nb + y)

-- Add a wrapped value to another using <>
instance Semigroup (Maybe' Integer) where
    Just' x <> Just' y = Just' (x + y)
    Just' x <> Nothing' = Just' x

-- Apply a function to a wrapped value using fmap or <$>
instance Functor Maybe' where
    fmap f Nothing' = Nothing'
    fmap f (Just' a) = Just' (f a)

-- Apply a wrapped function to a wrapped value using <*> or liftA
instance Applicative Maybe' where
    pure x = Just' x
    Just' f <*> Nothing' = Nothing'
    Just' f <*> Just' x = Just' (f x) 

-- Apply a function that returns a wrapped value, to a wrapped value using >>= or liftM
instance Monad Maybe' where 
    Nothing' >>= f = Nothing'
    Just' x >>= f = f x

-- A monoid is a semigroup with on additional requirement: the identity value
instance Monoid (Maybe' Integer) where
    mempty = Nothing'




