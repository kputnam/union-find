module Data.UnionFind.Array
  ( newUF
  , boxed
  , unboxed
  , union
  , find
  , partition
  ) where

import Data.Array.ST
import Control.Monad.ST
import qualified Data.Map as M

type UF s a
  = (s a a, s a a)

-- | Construct @n@ equivalence classes, each containing a single
--   element in @0..n-1@
newUF :: (Ix a, Num a, Enum a, MArray s a m) => a -> m (UF s a)
newUF n = do
  ps <- newListArray (0, n-1) [0..n-1]  -- parent is self
  sz <- newArray (0, n-1) 1             -- singletons
  return (ps, sz)

-- | Cast the result of `newUF` to a boxed array representation
boxed :: ST s (UF (STArray s) a) -> ST s (UF (STArray s) a)
boxed x = x

-- | Cast the result of `newUF` to an unboxed array representation
unboxed :: ST s (UF (STUArray s) a) -> ST s (UF (STUArray s) a)
unboxed x = x

-- | The representative element of @k@'s equivalence class
root :: (Ix a, MArray s a m) => UF s a -> a -> m a
root u@(ps, sz) k = do
  pk <- readArray ps k
  if pk == k
    then return pk
    else root u pk

-- | Join the equivalence classes of @j@ and @k@
union :: (Ix a, Num a, MArray s a m) => UF s a -> a -> a -> m (UF s a)
union u@(ps, sz) j k = do
  rj <- root u j
  rk <- root u k
  sj <- readArray sz rj
  sk <- readArray sz rk
  case sj `compare` sk of
    LT -> link u rj rk (sj + sk)
    _  -> link u rk rj (sj + sk)
  where
    link (ps, sz) j k n = do
      writeArray ps j k -- link j to k
      writeArray sz k n -- set k's size
      return (ps, sz)

-- | @True@ if @j@ and @k@ belong to the same equivalence class
find :: (Ix a, MArray s a m) => UF s a -> a -> a -> m Bool
find u j k = do
  rj <- root u j
  rk <- root u k
  return $ rj == rk

-- | Disjoint and non-empty subsets
partition :: (Num a, Ix a, MArray s a m) => UF s a -> m [[a]]
partition u@(ps, _) = do
  (_, k) <- getBounds ps
  loop k M.empty
  where
    loop k cs
      | k < 0     = return (M.elems cs)
      | otherwise = do r <- root u k
                       loop (k-1) (M.alter (merge k) r cs)
    merge k (Just xs) = Just (k:xs)
    merge k _         = Just [k]
