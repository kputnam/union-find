module Data.UnionFind.Array
  ( newUF
  , boxed
  , unboxed
  , union
  , find
  , partition
  ) where

import Data.Monoid
import qualified Data.Map as M

import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive

import qualified Data.Vector.Generic.Mutable    as V
import qualified Data.Vector.Fusion.Stream      as S
import qualified Data.Vector.Fusion.Stream.Size as S
import qualified Data.Vector                    as B
import qualified Data.Vector.Unboxed            as U

-- First array stores the parents of each element and the second
-- array stores the number of elements in each equivalence class
type UF v a
  = (v a, v a)

-- | Construct @n@ equivalence classes, each containing a single
--   element in @0..n-1@
newUF :: (PrimMonad m, Integral a, V.MVector v a)
      => a -> m (UF (v (PrimState m)) a)
newUF n = liftM2 (,) es sz
  where
    m  = fromIntegral n
    es = V.unstream $ S.sized (S.generate m fromIntegral) (S.Exact m)
    sz = V.replicate m 0

-- | Cast the result of `newUF` to a boxed array representation
boxed :: ST s (UF (B.MVector s) a) -> ST s (UF (B.MVector s) a)
boxed x = x

-- | Cast the result of `newUF` to an unboxed array representation
unboxed :: U.Unbox a => ST s (UF (U.MVector s) a) -> ST s (UF (U.MVector s) a)
unboxed x = x

-- | The representative element of @k@'s equivalence class
root :: (PrimMonad m, Integral a, V.MVector v a)
     => UF (v (PrimState m)) a -> a -> m a
root u@(ps, sz) k = flatten k =<< V.read ps (fromIntegral k)
  where
    flatten k pk
      | k == pk   = return pk
      | otherwise = do
          gk <- V.read ps (fromIntegral pk) -- parent of pk, grandparent of k
          V.write ps (fromIntegral k) gk    -- link k to its grandparent
          flatten pk gk

-- | Join the equivalence classes of @j@ and @k@
union :: (PrimMonad m, Integral a, V.MVector v a)
      => UF (v (PrimState m)) a -> a -> a -> m ()
union u@(ps, sz) j k = do
  rj <- root u j
  rk <- root u k
  sj <- V.read sz (fromIntegral rj)
  sk <- V.read sz (fromIntegral rk)
  case sj `compare` sk of
    LT -> link u rj rk (sj + sk)
    _  -> link u rk rj (sj + sk)
  where
    link (ps, sz) j k n = do
      V.write ps (fromIntegral j) k -- link j to k
      V.write sz (fromIntegral k) n -- set k's size

-- | @True@ if @j@ and @k@ belong to the same equivalence class
find :: (PrimMonad m, Integral a, V.MVector v a)
     => UF (v (PrimState m)) a -> a -> a -> m Bool
find u j k = do
  rj <- root u (fromIntegral j)
  rk <- root u (fromIntegral k)
  return $ rj == rk

-- | Enumerate each disjoint and non-empty subset
partition :: (PrimMonad m, Integral a, V.MVector v a)
          => UF (v (PrimState m)) a -> m [[a]]
partition u@(ps, _) = do
  loop (fromIntegral $ V.length ps - 1) M.empty
  where
    loop k cs
      | k < 0     = return (M.elems cs)
      | otherwise = do r <- root u k
                       loop (k-1) (M.insertWith (<>) r [k] cs)
