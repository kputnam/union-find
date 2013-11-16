module Data.UnionFind.Array
  ( UF
  , newUF
  , boxed
  , unboxed
  , union
  , find
  , partition
  ) where

import Data.Monoid
import Data.Array.ST
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
  ps <- newListArray (0, n-1) [0..n-1]  -- parent is self
  sz <- newArray (0, n-1) 1             -- singletons
  return $ UF ps sz

-- | Cast the result of `newUF` to a boxed array representation
boxed :: ST s (UF (STArray s) a) -> ST s (UF (STArray s) a)
boxed x = x

-- | Cast the result of `newUF` to an unboxed array representation
unboxed :: ST s (UF (STUArray s) a) -> ST s (UF (STUArray s) a)
unboxed x = x

-- | The representative element of @k@'s equivalence class
root :: (Ix a, MArray s a m) => UF s a -> a -> m a
root (UF ps _) k = flatten k =<< readArray ps k
  where
    flatten k pk
      | k == pk   = return pk
      | otherwise = do
          gk <- readArray ps pk -- parent of pk, grandparent of k
          writeArray ps k gk    -- link k to its grandparent
          flatten pk gk

-- | Join the equivalence classes of @j@ and @k@
union :: (Ix a, Num a, MArray s a m) => UF s a -> a -> a -> m ()
union u@(UF ps sz) j k = do
  rj <- root u j
  rk <- root u k
  sj <- readArray sz rj
  sk <- readArray sz rk
  case sj `compare` sk of
    LT -> link rj rk (sj + sk)
    _  -> link rk rj (sj + sk)
  where
    link j k n = do
      writeArray ps j k -- link j to k
      writeArray sz k n -- set k's size

-- | @True@ if @j@ and @k@ belong to the same equivalence class
find :: (Ix a, MArray s a m) => UF s a -> a -> a -> m Bool
find u j k = do
  rj <- root u j
  rk <- root u k
  return $ rj == rk

-- | Enumerate each disjoint and non-empty subset
partition :: (Num a, Ix a, MArray s a m) => UF s a -> m [[a]]
partition u@(UF ps _) = do
  (_, k) <- getBounds ps
  loop k M.empty
  where
    loop k cs
      | k < 0     = return (M.elems cs)
      | otherwise = do r <- root u k
                       loop (k-1) (M.insertWith (<>) r [k] cs)
