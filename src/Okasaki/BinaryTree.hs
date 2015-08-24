{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- 2.2 Binary Search Trees
module Okasaki.BinaryUnbalancedSet where

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
-- >>> insert 3 (T (T E 3 E) 8 E)
-- T (T E 3 E) 8 E
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

updateLeft :: UnbalancedSet a -> UnbalancedSet a -> UnbalancedSet a
updateLeft (T a y b) x = T x y b

updateRight :: UnbalancedSet a -> UnbalancedSet a -> UnbalancedSet a
updateRight (T a y b) x = T a y x

instance Ord a => Set UnbalancedSet a where
  empty = E

  -- Insert an element to a set.
  insert x E = T E x E
  insert x s@(T a y b)
    | x < y     = updateLeft s $ insert x a
    | x > y     = updateRight s $ insert x b
    | otherwise = s

  -- Return whether an element is a member of a set.
  member x E = False
  member x (T a y b)
    | x < y     = member x a
    | x > y     = member y a
    | otherwise = True

-- | Exercise 2.2
--
-- >>> member 3 $ CC E
-- False
--
-- >>> member 3 $ CC (T (T E 3 E) 8 E)
-- True
--
-- >>> member 5 $ CC (T (T E 3 E) 8 E)
-- False
newtype CacheCandidate a = CC (UnbalancedSet a)

instance Ord a => Set CacheCandidate a where
  empty = CC empty

  insert x (CC s) = CC $ insert x s

  member x (CC s) = member' x Nothing s
    where member' x Nothing E = False
          member' x (Just c) E = x == c
          member' x candidate (T a y b)
            | x < y     = member' x candidate a
            | otherwise = member' x (Just y) b

-- | Exercise 2.3
--
-- >>> insert 3 $ LC E
-- LC (T E 3 E)
--
-- >>> insert 5 $ LC (T (T E 3 E) 8 E)
-- LC (T (T E 3 (T E 5 E)) 8 E)
--
-- >>> insert 3 $ LC (T (T E 3 E) 8 E)
-- LC (T (T E 3 E) 8 E)
newtype LeastCopy a = LC (UnbalancedSet a) deriving (Show)

instance Ord a => Set LeastCopy a where
  empty = LC empty

  insert x (LC s) = case insert' x s of
    Nothing -> LC s
    Just ss -> LC ss
    -- TODO: Abuse of Maybe?
    where insert' x E = Just (T empty x empty)
          insert' x s@(T a y b)
            | x < y     = fmap (updateLeft s) $ insert' x a
            | x > y     = fmap (updateRight s) $ insert' x b
            | otherwise = Nothing

  member x (LC s) = member x s

-- | Exersize 2.4
--
-- >>> insert 3 $ LCCC E
-- LCCC (T E 3 E)
--
-- >>> insert 5 $ LCCC (T (T E 3 E) 8 E)
-- LCCC (T (T E 3 (T E 5 E)) 8 E)
--
-- >>> insert 3 $ LCCC (T (T E 3 E) 8 E)
-- LCCC (T (T E 3 E) 8 E)
newtype LeastCopyCacheCandidate a = LCCC (UnbalancedSet a) deriving (Show)

instance Ord a => Set LeastCopyCacheCandidate a where
  empty = LCCC empty

  insert x (LCCC s) = case insert' x Nothing s of
    Nothing -> LCCC s
    Just ss -> LCCC ss
    where insert' x Nothing E  = Just (T empty x empty)
          insert' x (Just c) E
            | x == c    = Nothing
            | otherwise = Just (T empty x empty)
          insert' x candidate s@(T a y b)
            | x < y     = fmap (updateLeft s) $ insert' x candidate a
            | otherwise = fmap (updateRight s) $ insert' x (Just y) b

  member x (LCCC s) = member x s

-- | Exercise 2.5 (a)
--
-- >>> complete 3 0
-- E
--
-- >>> complete 5 3
-- T (T (T E 5 E) 5 (T E 5 E)) 5 (T (T E 5 E) 5 (T E 5 E))
complete :: Ord a => a -> Int -> UnbalancedSet a
complete x d
  | d <= 0    = E
  | otherwise = let c = complete x (d - 1) in T c x c
