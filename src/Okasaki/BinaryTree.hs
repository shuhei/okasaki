{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- 2.2 Binary Search Trees
module Okasaki.BinaryUnbalancedSet where

import Prelude hiding (lookup, mempty)

class Set s a where
  empty :: s a
  insert :: a -> s a -> s a
  member :: a -> s a -> Bool

data UnbalancedSet a
  = E
  | T (UnbalancedSet a) a (UnbalancedSet a)
  deriving (Show)

updateLeft :: UnbalancedSet a -> UnbalancedSet a -> UnbalancedSet a
updateLeft (T a y b) x = T x y b

updateRight :: UnbalancedSet a -> UnbalancedSet a -> UnbalancedSet a
updateRight (T a y b) x = T a y x

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
-- Create a balanced tree of a given depth.
--
-- >>> complete 3 0
-- E
--
-- >>> complete 5 3
-- T (T (T E 5 E) 5 (T E 5 E)) 5 (T (T E 5 E) 5 (T E 5 E))
complete :: Ord a => a -> Word -> UnbalancedSet a
complete x d
  | d == 0    = E
  | otherwise = let c = complete x (d - 1) in T c x c

-- | Exercise 2.5 (b)
-- Create a balanced tree of a given size.
--
-- >>> create 2 0
-- E
--
-- >>> create 2 1
-- T E 2 E
--
-- >>> create 2 2
-- T (T E 2 E) 2 E
--
-- >>> create 2 3
-- T (T E 2 E) 2 (T E 2 E)
--
-- >>> create 2 4
-- T (T (T E 2 E) 2 E) 2 (T E 2 E)
create :: Ord a => a -> Word -> UnbalancedSet a
create x = fst . create2
  -- You can share subtrees even if you cannot share the whole tree!
  where create2 m
          | m == 0         = (E, T E x E)
          | m `mod` 2 == 1 = let (t0, t1) = create2 ((m - 1) `div` 2) in (T t0 x t0, T t1 x t0)
          | otherwise      = let (t0, t1) = create2 (m `div` 2 - 1) in (T t1 x t0, T t1 x t1)

-- | Exercise 2.6
class FiniteMap m k a where
  mempty :: m k a
  bind :: k -> a -> m k a -> m k a
  lookup :: k -> m k a -> Maybe a

data UnbalancedMap k a
  = EM
  | TM (UnbalancedMap k a) k a (UnbalancedMap k a)
  deriving (Show)

-- | Exercise 2.6
--
-- >>> mempty :: UnbalancedMap Word String
-- EM
--
-- >>> let kvs = [("foo", 123), ("bar", 234), ("baz", 345)]
-- >>> let mm = foldr (\(k, v) m -> bind k v m) mempty kvs :: UnbalancedMap String Int
-- >>> lookup "bar" mm
-- Just 234
-- >>> lookup "hello" mm
-- Nothing
instance (Ord k, Show k, Show a) => FiniteMap UnbalancedMap k a where
  mempty = EM

  bind k v EM = TM EM k v EM
  bind k v s@(TM a kk vv b)
    | k < kk     = TM (bind k v a) kk vv b
    | k > kk     = TM a kk vv (bind k v b)
    | otherwise = s

  lookup _ EM = Nothing
  lookup x (TM l y v r)
    | x < y     = lookup x l
    | x > y     = lookup x r
    | otherwise = Just v
