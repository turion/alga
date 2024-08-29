{-# LANGUAGE NamedFieldPuns #-}
module Algebra.Graph.Test.Dijkstra where

import Algebra.Graph.Labelled
import Algebra.Graph.Test (test)
import Control.Monad (guard)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Tree e v = Tree
  { value :: v
  , children :: [(e, Tree e v)]
  }

data Info e v = Info
  { dist :: Map v e
  , predecessor :: Map v v
  }

tree :: Info e v -> Tree e v
tree = _

-- Nothing represents no connection
dijkstra' :: (Num e, Eq v) => v -> Graph (Maybe e) v -> Tree e v -> Tree e v
dijkstra' v Empty t = t
dijkstra' v (Vertex _) t = t
dijkstra' v (Connect e (Vertex v') g2) _
  | v == v' = _
  | otherwise = _
dijkstra' v (Connect e g1 g2) _ = _

neighbours :: (Ord v, Ord e) => v -> Graph (Maybe e) v -> Set (e, v)
neighbours v Empty = Set.empty
neighbours v (Vertex _) = Set.empty
neighbours v (Connect Nothing g1 g2) = neighbours v g1 <> neighbours v g2
neighbours v (Connect (Just e) g1 g2) =
  if v `elem` vertexList g1
    then Set.fromList $ (e,) <$> vertexList g2
    else Set.empty

dijkstra2 :: forall v e. (Num e, Ord e, Ord v) => v -> Graph (Maybe e) v -> Info e v
dijkstra2 v g = loop (Set.singleton (0, v)) $ Info { dist = Map.singleton v 0, predecessor = Map.empty }
 where
  loop :: Set (e, v) -> Info e v -> Info e v
  loop q t = maybe t (\((e, v), q') -> loop (q' <> _) (foldl' maybeAdd t (neighbours v g))) $ Set.minView q
  maybeAdd :: Info e v -> (e, v) -> Info e v
  maybeAdd Info {dist, predecessor} (e, v) = _


testDijkstra :: IO ()
testDijkstra = test "Dijkstra" True
