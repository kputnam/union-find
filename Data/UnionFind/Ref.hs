module Data.UnionFind.Ref
  ( Class
  , label
  , weight
  , singleton
  , weighted
  , union
  , find
  , connected
  ) where

import Data.Maybe
import Data.STRef
import Data.Semigroup
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
--   liftA3 (,,) (label a) (label b) (label d)
--

data Class s m a =
  Class
  { label_  :: a
  , parent  :: STRef s (Maybe (Class s m a))
  , weight_ :: STRef s m }

label :: Class s m a -> ST s a
label = fmap label_ . find

weight :: Class s m a -> ST s m
weight k = readSTRef . weight_ =<< find k

-- Elements in the graph are uniquely identified by their `parent` field, which
-- is a reference to another Class. We're not comparing what is being referred
-- to (which changes), but the references themselves (which do not change).
--
--   let a = singleton 'a' in
--     runST $ (\x -> x == x) a -- True
--     runST $ liftA2 (==) a a  -- False
--
instance Eq (Class s m a) where
  a == b = parent a == parent b

singleton :: a -> ST s (Class s (Sum Int) a)
singleton a = Class a <$> newSTRef Nothing <*> newSTRef mempty

weighted :: (Semigroup m, Ord m) => m -> a -> ST s (Class s m a)
weighted m a = Class a <$> newSTRef Nothing <*> newSTRef m

-- | Returns a reference to the representative element of @k@'s equivalence class.
--   The given class's `parent` reference may be updated (to reduce indirection).
--   Runs in O(1) amortized time.
find :: Class s m a -> ST s (Class s m a)
find k = aux k =<< readSTRef (parent k)
  where
    aux k Nothing  = return k
    aux k (Just p) = do
      -- Does k have a grandparent?
      g <- readSTRef (parent p)
      case g of
        Nothing ->
          return p                -- No, p is the end of the path
        Just _ -> do
          writeSTRef (parent k) g -- Yes, link k to g, skipping over p,
          aux p g                 -- then try linking p to its grandparent

-- | Join the equivalence classes of @j@ and @k@, and return a reference to the
--   element representing the class containing @j@ and @k@. Runs in O(1) amortized
--   time.
union :: (Semigroup m, Ord m) => Class s m a -> Class s m a -> ST s (Class s m a)
union j k
  | j == k    = return j
  | otherwise = do
      rootJ <- find j
      rootK <- find k

      -- Check if these are disjoint classes
      if rootJ /= rootK
        then choose rootJ rootK
        else return rootJ
  where
    choose rootJ rootK = do
      weightJ <- readSTRef (weight_ rootJ)
      weightK <- readSTRef (weight_ rootK)

      -- Update the smaller class to point to the larger
      case weightJ `compare` weightK of
        LT -> link rootJ rootK (weightJ <> weightK)
        _  -> link rootK rootJ (weightJ <> weightK)

    link small large m = do
      writeSTRef (parent small) (Just large)
      writeSTRef (weight_ large) m
      return large

-- | @True@ if @j@ and @k@ belong to the same equivalence class. Runs
--   in O(1) amortized time.
connected :: Class s m a -> Class s m a -> ST s Bool
connected j k
  | j == k    = return True -- trivial case
  | otherwise = do
      -- Maybe (Class s m a)
      parentJ <- readSTRef (parent j)
      parentK <- readSTRef (parent k)

      -- If either parent is Nothing, then j or k were unequal representative elements
      -- (remember, we know j /= k from earlier). Otherwise, see if they refer to the
      -- same representative element.
      fromMaybe (return False) (compare <$> parentJ <*> parentK)
  where
    compare :: Class s m a -> Class s m a -> ST s Bool
    compare j k = (==) <$> find j <*> find k
