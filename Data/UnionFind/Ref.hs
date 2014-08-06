module Data.UnionFind.Ref
  ( singleton
  , union
  , find
  , connected
  ) where

import Data.Maybe
import Data.STRef
import Control.Monad.ST
import Control.Applicative

data Class s a =
  Class
  { label  :: a
  , parent :: STRef s (Maybe (Class s a))
  , size   :: STRef s Int
  }

-- Elements in the graph are uniquely identified by their `parent` field, which
-- is a reference to another Class. We're not comparing what is being referred
-- to (which changes), but the references themselves (which do not change).
--
--   let a = singleton 'a' in
--     runST $ (\x -> x == x) a -- True
--     runST $ liftA2 (==) a a  -- False
--
instance Eq (Class s a) where
  a == b = parent a == parent b

-- Non-optional reference to a Class (dereferencing it never results in Nothing)
type Ref s a
  = STRef s (Class s a)

singleton :: a -> ST s (Class s a)
singleton a = Class a <$> newSTRef Nothing <*> newSTRef 1

-- | Returns a reference to the representative element of @k@'s equivalence class.
--   The given reference may be updated (to flatten indirect references). Runs in
--   O(1) amortized time.
find :: Ref s a -> ST s (Ref s a)
find k = undefined

-- | Join the equivalence classes of @j@ and @k@, and return a reference to the
--   element representing the class containing @j@ and @k@. Runs in O(1) amortized time.
union :: Class s a -> Class s a -> ST s (Ref s a)
union j k = undefined

-- | @True@ if @j@ and @k@ belong to the same equivalence class. Runs
--   in O(1) amortized time.
connected :: Class s a -> Class s a -> ST s Bool
connected j k
  | j == k    = return True -- trivial case
  | otherwise = do
      -- Maybe (Ref s a)
      parentJ <- deref (parent j)
      parentK <- deref (parent k)

      -- If either parent is Nothing, then j or k were unequal representative elements
      -- (remember, we know j /= k from earlier). Otherwise, see if they refer to the
      -- same representative element.
      fromMaybe (return False) (compare <$> parentJ <*> parentK)
  where
    deref :: STRef s (Maybe (Class s a)) -> ST s (Maybe (Ref s a))
    deref = undefined

    compare :: Ref s a -> Ref s a -> ST s Bool
    compare j k = (==) <$> find j <*> find k

