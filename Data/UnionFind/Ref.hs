module Data.UnionFind.Ref
  ( Class ( label )
  , singleton
  , union
  , find
  , connected
  ) where

import Data.Maybe
import Data.STRef
import Control.Monad.ST
import Control.Applicative

--
-- runST $ do
--   a <- singleton 'a'
--   b <- singleton 'b'
--   c <- singleton 'c'
--   d <- singleton 'd'
--
--   union a d
--   union b a
--
--   liftA3 (,,) (label <$> find a) (label <$> find b) (label <$> find d)
--

data Class s a =
  Class
  { label  :: a
  , parent :: STRef s (Maybe (Class s a))
  , size   :: STRef s Int }

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

singleton :: a -> ST s (Class s a)
singleton a = Class a <$> newSTRef Nothing <*> newSTRef 1

-- | Returns a reference to the representative element of @k@'s equivalence class.
--   The given class's `parent` reference may be updated (to reduce indirection).
--   Runs in O(1) amortized time.
find :: Class s a -> ST s (Class s a)
find k = aux k =<< readSTRef (parent k)
  where
    aux k Nothing  = return k
    aux k (Just p) = do
      -- Does k have a grandparent?
      g <- readSTRef (parent p)
      case g of
        Nothing ->
          return p                -- No, p is the end of the path
        Just g' -> do
          writeSTRef (parent k) g -- Yes, link k to g, skipping over p,
          aux p g                 -- then try linking p to its grandparent

-- | Join the equivalence classes of @j@ and @k@, and return a reference to the
--   element representing the class containing @j@ and @k@. Runs in O(1) amortized
--   time.
union :: Class s a -> Class s a -> ST s (Class s a)
union j k
  | j == k    = return j
  | otherwise = do
      rootJ <- find j
      rootK <- find k

      -- Check if these are disjoint classes
      if rootJ /= rootK
      then do
        sizeJ <- readSTRef (size rootJ)
        sizeK <- readSTRef (size rootK)

        -- Update the smaller class to point to the larger
        case sizeJ `compare` sizeJ of
          LT -> link rootJ rootK (sizeJ + sizeK)
          _  -> link rootK rootJ (sizeJ + sizeK)
      else return rootJ
  where
    link :: Class s a -> Class s a -> Int -> ST s (Class s a)
    link small large n = do
      writeSTRef (parent small) (Just large)
      writeSTRef (size large) n
      return large

-- | @True@ if @j@ and @k@ belong to the same equivalence class. Runs
--   in O(1) amortized time.
connected :: Class s a -> Class s a -> ST s Bool
connected j k
  | j == k    = return True -- trivial case
  | otherwise = do
      -- Maybe (Class s a)
      parentJ <- readSTRef (parent j)
      parentK <- readSTRef (parent k)

      -- If either parent is Nothing, then j or k were unequal representative elements
      -- (remember, we know j /= k from earlier). Otherwise, see if they refer to the
      -- same representative element.
      fromMaybe (return False) (compare <$> parentJ <*> parentK)
  where
    compare :: Class s a -> Class s a -> ST s Bool
    compare j k = (==) <$> find j <*> find k

