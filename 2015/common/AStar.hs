module AStar(
  AStep(..),
  aStarOn
) where

import PQueue qualified as PQueue
import Data.Set qualified as Set
import Data.Foldable (foldl')
import Debug.Trace(trace)

data AStep a p = AStep {
  stepNext :: a,
  stepCost :: p,
  stepHeuristic :: p
} deriving (Show)

data ACost a p = ACost !p a deriving (Show)

aStarOn :: (Enum p, Num p, Ord r) 
  => (a -> r)            -- representation function for unicity
  -> (a -> [AStep a p])  -- step function
  -> [a]                 -- Starting nodes
  -> [(a, p)]            -- Visited nodes in order of cost.
aStarOn rep step starts = go Set.empty $ PQueue.fromList [(toEnum 0, ACost (toEnum 0) s) | s <- starts]
  where go !visited pq = 
          case PQueue.minView pq of 
            Nothing                     -> []
            Just ((p, ACost c v), pq')  -> 
              if Set.member r visited 
                then go visited pq'
                else (v, c):go visited' pq''
              where r         = rep v
                    visited'  = Set.insert r visited
                    pq''      = foldl' addWork pq' (step v)
                    addWork x a = PQueue.insert (c' + stepHeuristic a) (ACost c' (stepNext a)) x
                      where c' = c + stepCost a



  