{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- 3.1 Leftist Heaps
module Okasaki.LeftistHeap where

class Heap h a where
  empty :: h a
  isEmpty :: h a -> Bool

  insert :: a -> h a -> h a
  merge :: h a -> h a -> h a

  findMin :: h a -> Maybe a
  deleteMin :: h a -> Maybe (h a)

data LeftistHeap a
  = E
  | T Word a (LeftistHeap a) (LeftistHeap a)
  deriving (Show, Eq)

rank E = 0
rank (T r _ _ _) = r

makeT x a b
  | rank a >= rank b = T (rank b + 1) x a b
  | otherwise        = T (rank a + 1) x b a

-- | Leftist heap.
--
-- >>> findMin (empty :: LeftistHeap Int)
-- Nothing
--
-- >>> let m = foldr insert empty [5, 3, 6, 2] :: LeftistHeap Int
-- >>> findMin m
-- Just 2
-- >>> deleteMin m >>= deleteMin >>= findMin
-- Just 5
instance (Ord a) => Heap LeftistHeap a where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  insert x h = merge (T 1 x E E) h

  merge h E = h
  merge E h = h
  merge h1@(T _ x a1 b1) h2@(T _ y a2 b2)
    | x <= y    = makeT x a1 $ merge b1 h2
    | otherwise = makeT y a2 $ merge h1 b2

  findMin E = Nothing
  findMin (T _ x _ _) = Just x

  deleteMin E = Nothing
  deleteMin (T _ _ a b) = Just (merge a b)

-- Exercise 3.1
-- Given a hash with n nodes, the right spine is longest when other paths are shortest. From the leftist propertty, the right spine is the shortest in a hash. Thus, the right spine has most elements when the heap is balanced.
-- For a hash with n nodes and the right spine of l elements, n >= 2 ^ l - 1. And that implies l <= log (n + 1).

-- | Exercise 3.2
--
-- >>> let dih = insert 8 $ insert 4 $ insert 5 $ insert 3 empty :: DirectInsert Int
-- >>> let h = insert 8 $ insert 4 $ insert 5 $ insert 3 empty :: LeftistHeap Int
-- >>> DI h == dih
-- True
newtype DirectInsert a = DI (LeftistHeap a) deriving (Show, Eq)

instance (Ord a) => Heap DirectInsert a where
  empty = DI empty
  isEmpty (DI h) = isEmpty h
  merge (DI h1) (DI h2) = DI $ merge h1 h2
  findMin (DI h) = findMin h
  deleteMin (DI h) = fmap DI $ deleteMin h

  insert x (DI h) = DI $ insert' x h
    where insert' x E = T 1 x E E
          insert' x h@(T r y a b)
            | x <= y    = makeT x E h
            | otherwise = makeT y a $ insert' x b
