{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- 2.2 Binary Search Trees
module Okasaki.BinaryTree
     ( Set (..)
     , UnbalancedSet (..)
     ) where

class Set s a where
  empty :: s a
  insert :: a -> s a -> s a
  member :: a -> s a -> Bool

-- |
-- >>> empty :: UnbalancedSet Int
-- E
--
-- >>> insert 3 E
-- T E 3 E
--
-- >>> insert 5 (T (T E 3 E) 8 E)
-- T (T E 3 (T E 5 E)) 8 E
--
-- >>> member 3 (T (T E 3 E) 8 E)
-- True
--
-- >>> member 5 (T (T E 3 E) 8 E)
-- False
data UnbalancedSet a
  = E
  | T (UnbalancedSet a) a (UnbalancedSet a)
  deriving (Show)

instance Ord a => Set UnbalancedSet a where
  -- | Return an empty set.
  empty = E

  -- | Insert an element to a set.
  insert x E = T E x E
  insert x s@(T a y b)
    | x < y     = T (insert x a) y b
    | x > y     = T a y (insert x b)
    | otherwise = s

  -- | Excersize 2.2
  -- Return whether an element is a member of a set.
  --
  -- Simple implementatin:
  -- > member x E = False
  -- > member x (T a y b)
  -- >   | x < y     = member x a
  -- >   | x > y     = member y a
  -- >   | otherwise = True
  member x E = False
  member x (T a y b) = member' x y (T a y b)
    where member' x c E = x == c
          member' x c (T a y b)
            | x < y     = member' x c a
            | otherwise = member' x y b
