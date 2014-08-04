module Data.UnionFind.Vector
  ( UF
  , newUF
  , boxed
  , unboxed
  , union
  , find
  , connected
  , partition
  , toList
  , fromList
  , freeze
  , thaw
  ) where

import Data.Monoid
import qualified Data.Map as M

import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive

import qualified Data.Vector.Generic            as G
import qualified Data.Vector.Generic.Mutable    as V
import qualified Data.Vector.Fusion.Stream      as S
import qualified Data.Vector.Fusion.Stream.Size as S
import qualified Data.Vector                    as B
import qualified Data.Vector.Unboxed            as U

-- First vector stores the parents of each element and the second
-- vector stores the number of elements in each equivalence class
--
-- runST $ do
--   uf <- unboxed (newUF (10 :: Int))
--   union uf 3 4
--   union uf 4 9
--   union uf 8 0
--   union uf 2 3
--   union uf 5 6
--   union uf 5 9
--   union uf 7 3
--   freeze uf
data UF v a
  = UF (v a) -- parents
       (v a) -- sizes
  deriving (Eq, Show, Read)

-- | Construct @n@ equivalence classes, each containing a single
--   element in @0..n-1@
newUF :: (PrimMonad m, Integral a, V.MVector v a)
      => a -> m (UF (v (PrimState m)) a)
newUF n = liftM2 UF parents sizes
  where
    m       = fromIntegral n
    parents = V.unstream $ S.sized (S.generate m fromIntegral) (S.Exact m)
    sizes   = V.replicate m 1

-- | Construct equivalence classes from pairs of equivalent elements.
--   Runs in O(n) time, where n is the number of given pairs.
fromList :: (PrimMonad m, Integral a, V.MVector v a)
         => a -> [(a, a)] -> m (UF (v (PrimState m)) a)
fromList n xs = do
  u <- newUF n
  mapM_ (uncurry $ union u) xs
  return u

-- | Restrict the result of `newUF` to a boxed vector representation
boxed :: ST s (UF (B.MVector s) a) -> ST s (UF (B.MVector s) a)
boxed x = x

-- | Restrict the result of `newUF` to an unboxed vector representation
unboxed :: U.Unbox a => ST s (UF (U.MVector s) a) -> ST s (UF (U.MVector s) a)
unboxed x = x

-- | The representative element of @k@'s equivalence class. Runs in O(1)
--   amortized time.
find :: (PrimMonad m, Integral a, V.MVector v a)
     => UF (v (PrimState m)) a -> a -> m a
find (UF parents _) k = flatten k =<< V.read parents (fromIntegral k)
  where
    flatten k pk
      | k == pk   = return pk
      | otherwise = do
          gk <- V.read parents (fromIntegral pk) -- parent of pk, grandparent of k
          V.write parents (fromIntegral k) gk    -- link k to its grandparent
          flatten pk gk

-- | Join the equivalence classes of @j@ and @k@, and return the element
--   representing the class containing @j@ and @k@. Runs in O(1) amortized time.
union :: (PrimMonad m, Integral a, V.MVector v a)
      => UF (v (PrimState m)) a -> a -> a -> m a
union uf@(UF parents sizes) j k = do
  rootJ <- find uf j
  rootK <- find uf k

  sizeJ <- V.read sizes (fromIntegral rootJ)
  sizeK <- V.read sizes (fromIntegral rootK)

  case sizeJ `compare` sizeK of
    LT -> link rootJ rootK (sizeJ + sizeK)
    _  -> link rootK rootJ (sizeJ + sizeK)

  where
    link j k n = do
      V.write parents (fromIntegral j) k -- link j to k
      V.write sizes   (fromIntegral k) n -- set k's size
      return k

-- | @True@ if @j@ and @k@ belong to the same equivalence class. Runs
--   in O(1) amortized time.
connected :: (PrimMonad m, Integral a, V.MVector v a)
        => UF (v (PrimState m)) a -> a -> a -> m Bool
connected uf j k = do
  rootJ <- find uf j
  rootK <- find uf k
  return $ rootJ == rootK

-- | Pair each element with the representative element of its equivalence
--   class. Runs in O(n) where n is the number of elements.
toList :: (PrimMonad m, Integral a, V.MVector v a)
       => UF (v (PrimState m)) a -> m [(a, a)]
toList uf@(UF parents _) = loop [] (fromIntegral $ V.length parents - 1)
  where
    loop xs k | k < 0     = return xs
              | otherwise = do rootK <- find uf k
                               loop ((k, rootK):xs) (k-1)

-- | Enumerate each disjoint and non-empty subset.
partition :: (PrimMonad m, Integral a, V.MVector v a)
          => UF (v (PrimState m)) a -> m [[a]]
partition = liftM (groups . map single) . toList
  where
    single (k, root) = (root, [k])
    groups = M.elems . M.fromListWith (<>)

-- | Create an immutable copy of the mutable UF
freeze :: (PrimMonad m, G.Vector v a)
       => UF (G.Mutable v (PrimState m)) a -> m (UF v a)
freeze (UF parents sizes) = liftM2 UF (G.freeze parents) (G.freeze sizes)

-- | Create a mutable copy of the immutable UF
thaw :: (PrimMonad m, Integral a, G.Vector v a)
     => UF v a -> m (UF (G.Mutable v (PrimState m)) a)
thaw (UF parents sizes) = liftM2 UF (G.thaw parents) (G.thaw sizes)
