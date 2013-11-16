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
  ps <- newListArray (0, n-1) [0..n-1]  -- parent is self
  sz <- newArray (0, n-1) 1             -- singletons
  return $ UF ps sz

-- |
fromList :: (Integral a, Ix a, MArray s a m) => a -> [(a, a)] -> m (UF s a)
fromList n xs = do
  u <- newUF n
  mapM_ (uncurry $ union u) xs
  return u

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

toList :: (Num a, Ix a, MArray s a m) => UF s a -> m [(a, a)]
toList u@(UF ps _) = loop [] . snd =<< getBounds ps
  where
    loop xs k | k < 0     = return xs
              | otherwise = do r <- root u k
                               loop ((k,r):xs) (k-1)

-- | Enumerate each disjoint and non-empty subset
partition :: (Num a, Ix a, MArray s a m) => UF s a -> m [[a]]
partition u = liftM (groups . map single) (toList u)
  where
    single (k, r) = (r, [k])
    groups = M.elems . M.fromListWith (<>)
