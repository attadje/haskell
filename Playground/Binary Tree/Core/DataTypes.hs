module Core.DataTypes where

data BTree a =  Node a (BTree a) (BTree a) | L (BTree a) | R (BTree a) | Empty deriving (Show)

instance Functor BTree  where
    fmap f (Node x (L Empty) (R Empty)) = Node (f x) (L Empty) (R Empty)
    fmap f (Node x (L Empty) (R xss)) = Node (f x) (L Empty) (R $ fmap f xss)
    fmap f (Node x (L xs) (R Empty)) = Node (f x) (L $ fmap f xs) (R Empty)
    fmap f (Node x (L xs) (R xss)) = Node (f x) (L $ fmap f xs) (R $ fmap f xss)
    
instance Applicative BTree where
    pure x = Node x (L Empty) (R Empty)
    Node f (L Empty) (R Empty) <*> Node x (L xs) (R xss) = Node (f x) (L (fmap f xs)) (R (fmap f xss))
    
instance Monad BTree where
    Node x (L _) (R _) >>= f = f x
    Empty >>= _ = Empty