module Data.UnionFind.Array
  ( UF
  , newUF
  , boxed
  , unboxed
  , union
  , find
  , partition
  , toList
  , fromList
  ) where

import Data.Monoid
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import qualified Data.Map as M

-- First array stores the parents of each element and the second
-- array stores the number of elements in each equivalence class
data UF s a
  = UF (s a a) (s a a)

-- | Construct @n@ equivalence classes, each containing a single
--   element in @0..n-1@
newUF :: (Ix a, Num a, Enum a, MArray s a m) => a -> m (UF s a)
newUF n = do
  parents <- newListArray (0, n-1) [0..n-1]  -- parent is self
  sizes   <- newArray (0, n-1) 1             -- singletons
  return $ UF parents sizes

-- |
fromList :: (Integral a, Ix a, MArray s a m) => a -> [(a, a)] -> m (UF s a)
fromList n xs = do
  uf <- newUF n
  mapM_ (uncurry $ union uf) xs
  return uf

-- | Cast the result of `newUF` to a boxed array representation
boxed :: ST s (UF (STArray s) a) -> ST s (UF (STArray s) a)
boxed x = x

-- | Cast the result of `newUF` to an unboxed array representation
unboxed :: ST s (UF (STUArray s) a) -> ST s (UF (STUArray s) a)
unboxed x = x

-- | The representative element of @k@'s equivalence class
root :: (Ix a, MArray s a m) => UF s a -> a -> m a
root (UF parents _) k = flatten k =<< readArray parents k
  where
    flatten k pk
      | k == pk   = return pk
      | otherwise = do
          gk <- readArray parents pk -- parent of pk, grandparent of k
          writeArray parents k gk    -- link k to its grandparent
          flatten pk gk

-- | Join the equivalence classes of @j@ and @k@
union :: (Ix a, Num a, MArray s a m) => UF s a -> a -> a -> m a
union uf@(UF parents sizes) j k = do
  rootJ <- root uf j
  rootK <- root uf k
  sizeJ <- readArray sizes rootJ
  sizeK <- readArray sizes rootK
  case sizeJ `compare` sizeK of
    LT -> link rootJ rootK (sizeJ + sizeK)
    _  -> link rootK rootJ (sizeJ + sizeK)
  where
    link j k n = do
      writeArray parents j k -- link j to k
      writeArray sizes   k n -- set k's size
      return k

-- | @True@ if @j@ and @k@ belong to the same equivalence class
find :: (Ix a, MArray s a m) => UF s a -> a -> a -> m Bool
find uf j k = do
  rootJ <- root uf j
  rootK <- root uf k
  return $ rootJ == rootK

toList :: (Num a, Ix a, MArray s a m) => UF s a -> m [(a, a)]
toList uf@(UF parents _) = loop [] . snd =<< getBounds parents
  where
    loop xs k | k < 0     = return xs
              | otherwise = do r <- root uf k
                               loop ((k,r):xs) (k-1)

-- | Enumerate each disjoint and non-empty subset
partition :: (Num a, Ix a, MArray s a m) => UF s a -> m [[a]]
partition = liftM (groups . map single) . toList
  where
    single (k, root) = (root, [k])
    groups = M.elems . M.fromListWith (<>)
