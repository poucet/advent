
{-# Language PatternSynonyms, ViewPatterns, DeriveTraversable #-}
module PQueue (
  PQueue,
  empty, 
  singleton,
  fromList,
  insert,
  minView,
)where

import Data.IntMap (IntMap)
import Data.IntMap.Strict qualified as IntMap

-- A priority queue with as priority type 'p' and value type 'a'.
newtype PQueue p a = PQ (IntMap [a])
  deriving (Functor, Foldable, Traversable)

instance (Enum p, Show p, Show a) => Show (PQueue p a) where
  showsPrec prec (PQ q) = showParen (prec >= 11) $ showString "fromList " . shows [(toEnum p :: p, v) | (p, vs) <- IntMap.toList q, v <- vs] 

empty :: PQueue p a 
empty = PQ IntMap.empty

singleton :: (Enum p) => p -> a -> PQueue p a
singleton p v = PQ (IntMap.singleton (fromEnum p) [v])

insert :: (Enum p) => p -> a -> PQueue p a -> PQueue p a
insert p v (PQ q) = PQ $ IntMap.alter helper (fromEnum p) q
  where helper Nothing = Just [v]
        helper (Just vs) = Just (v:vs)


minView :: (Enum p) => PQueue p a -> Maybe ((p, a), PQueue p a)
minView (PQ q) = do
  ((p, vs), q') <- IntMap.minViewWithKey q
  case vs of 
    []      -> Nothing
    [v]     -> Just ((toEnum p, v), PQ q')
    (v:vs') -> let q'' = IntMap.insert p vs' q' 
                in q'' `seq` Just ((toEnum p, v), PQ $ q'')

fromList :: (Enum p) => [(p, a)] -> PQueue p a
fromList xs = PQ (IntMap.fromListWith (flip (++)) [(fromEnum p, [v]) | (p, v) <- xs ])
