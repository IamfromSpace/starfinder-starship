module Data.KeyedSet where

import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Prelude hiding (lookup)

data KeyedSet k v = KeyedSet (v -> k) (HM.HashMap k v)

insert :: (Eq k, Hashable k) => v -> KeyedSet k v -> KeyedSet k v
insert v (KeyedSet fn hm) = KeyedSet fn $ HM.insert (fn v) v hm

delete :: (Eq k, Hashable k) => k -> KeyedSet k v -> KeyedSet k v
delete k (KeyedSet fn hm) = KeyedSet fn  $ HM.delete k hm

lookup :: (Eq k, Hashable k) => k -> KeyedSet k v -> Maybe v
lookup k (KeyedSet _ hm) = HM.lookup k hm

fromList :: (Eq k, Hashable k) => (v -> k) -> [v] -> KeyedSet k v
fromList fn = KeyedSet fn . HM.fromList . fmap (\v -> (fn v, v))
