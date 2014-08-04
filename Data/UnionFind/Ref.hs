module Data.UnionFind.Ref
  ( singleton
  , union
  , find
  , connected
  ) where

import Data.STRef
import Control.Monad.ST
import Control.Applicative

data Class s a =
  Class
  { label  :: a
  , parent :: STRef s (Maybe (Class s a))
  , size   :: STRef s Int
  }

type Ref s a
  = STRef s (Class s a)

singleton :: a -> ST s (Class s a)
singleton a = Class a <$> newSTRef Nothing <*> newSTRef 1

-- | The representative element of @k@'s equivalence class. Runs in O(1)
--   amortized time.
find :: Class s a -> ST s (Ref s a)
find k = undefined

-- | Join the equivalence classes of @j@ and @k@, and return the element
--   representing the class containing @j@ and @k@. Runs in O(1) amortized time.
union :: Class s a -> Class s a -> ST s (Ref s a)
union j k = undefined

-- | @True@ if @j@ and @k@ belong to the same equivalence class. Runs
--   in O(1) amortized time.
connected :: Class s a -> Class s a -> ST s Bool
connected j k = undefined

