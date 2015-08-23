-- 2.1 Lists
module Okasaki.List
     ( suffixes
     ) where

-- | Excersize 2.1
-- Return suffixes of a list in descending order.
--
-- >>> suffixes [1,2,3,4]
-- [[1,2,3,4],[2,3,4],[3,4],[4]]
--
-- >>> suffixes []
-- []
suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes xss@(_:xs) = xss : suffixes xs
