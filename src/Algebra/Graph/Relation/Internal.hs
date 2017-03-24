-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of binary relations. The API is unstable
-- and unsafe. Where possible use non-internal modules "Algebra.Graph.Relation",
-- "Algebra.Graph.Relation.Reflexive", "Algebra.Graph.Relation.Symmetric",
-- "Algebra.Graph.Relation.Transitive" and "Algebra.Graph.Relation.Preorder"
-- instead.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Relation.Internal (
    -- * Data structure
    Relation (..), consistent,

    -- * Basic graph construction primitives
    empty, vertex, overlay, connect, vertices, edges, fromAdjacencyList,

    -- * Graph properties
    edgeList, preset, postset,

    -- * Graph transformation
    removeVertex, removeEdge, gmap, induce,

    -- * Operations on binary relations
    reflexiveClosure, symmetricClosure, transitiveClosure, preorderClosure,

    -- * Reflexive relations
    ReflexiveRelation (..),

    -- * Symmetric relations
    SymmetricRelation (..),

    -- * Transitive relations
    TransitiveRelation (..),

    -- * Preorders
    PreorderRelation (..)
  ) where

import Data.Tuple
import Data.Set (Set, union)

import qualified Algebra.Graph.Class as C
import qualified Data.Set            as Set

{-| The 'Relation' data type represents a graph as a /binary relation/. We define
a law-abiding 'Num' instance as a convenient notation for working with graphs:

    > 0           == vertex 0
    > 1 + 2       == overlay (vertex 1) (vertex 2)
    > 1 * 2       == connect (vertex 1) (vertex 2)
    > 1 + 2 * 3   == overlay (vertex 1) (connect (vertex 2) (vertex 3))
    > 1 * (2 + 3) == connect (vertex 1) (overlay (vertex 2) (vertex 3))

The 'Show' instance is defined using basic graph construction primitives:

@show ('empty'     :: Relation Int) == "empty"
show (1         :: Relation Int) == "vertex 1"
show (1 + 2     :: Relation Int) == "vertices [1,2]"
show (1 * 2     :: Relation Int) == "edge 1 2"
show (1 * 2 * 3 :: Relation Int) == "edges [(1,2),(1,3),(2,3)]"
show (1 * 2 + 3 :: Relation Int) == "graph [1,2,3] [(1,2)]"@

The 'Eq' instance satisfies all axioms of algebraic graphs:

    * 'overlay' is commutative and associative:

        >       x + y == y + x
        > x + (y + z) == (x + y) + z

    * 'connect' is associative and has 'empty' as the identity:

        >   x * empty == x
        >   empty * x == x
        > x * (y * z) == (x * y) * z

    * 'connect' distributes over 'overlay':

        > x * (y + z) == x * y + x * z
        > (x + y) * z == x * z + y * z

    * 'connect' can be decomposed:

        > x * y * z == x * y + x * z + y * z

The following useful theorems can be proved from the above set of axioms.

    * 'overlay' has 'empty' as the identity and is idempotent:

        >   x + empty == x
        >   empty + x == x
        >       x + x == x

    * Absorption and saturation of 'connect':

        > x * y + x + y == x * y
        >     x * x * x == x * x

When specifying the time and memory complexity of graph algorithms, /n/ and /m/
will denote the number of vertices and edges in the graph, respectively.
-}
data Relation a = Relation {
    -- | The /domain/ of the relation.
    domain :: Set a,
    -- | The set of pairs of elements that are /related/. It is guaranteed that
    -- each element belongs to the domain.
    relation :: Set (a, a)
  } deriving Eq

instance (Ord a, Show a) => Show (Relation a) where
    show (Relation d r)
        | vs == []     = "empty"
        | es == []     = if Set.size d > 1 then "vertices " ++ show vs
                                           else "vertex "   ++ show v
        | d == related = if Set.size r > 1 then "edges " ++ show es
                                           else "edge "  ++ show e ++ " " ++ show f
        | otherwise    = "graph " ++ show vs ++ " " ++ show es
      where
        vs      = Set.toAscList d
        es      = Set.toAscList r
        v       = head $ Set.toAscList d
        (e, f)  = head $ Set.toAscList r
        related = Set.fromList . uncurry (++) $ unzip es

instance Ord a => C.Graph (Relation a) where
    type Vertex (Relation a) = a
    empty   = empty
    vertex  = vertex
    overlay = overlay
    connect = connect

instance (Ord a, Num a) => Num (Relation a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

-- | Check if the internal representation of a relation is consistent, i.e. if all
-- pairs of elements in the 'relation' refer to existing elements in the 'domain'.
-- It should be impossible to create an inconsistent 'Relation', and we use this
-- function in testing.
--
-- @
-- consistent 'empty'                  == True
-- consistent ('vertex' x)             == True
-- consistent ('overlay' x y)          == True
-- consistent ('connect' x y)          == True
-- consistent ('Algebra.Graph.Relation.edge' x y)             == True
-- consistent ('edges' xs)             == True
-- consistent ('Algebra.Graph.Relation.graph' xs ys)          == True
-- consistent ('fromAdjacencyList' xs) == True
-- @
consistent :: Ord a => Relation a -> Bool
consistent r = Set.fromList (uncurry (++) $ unzip $ edgeList r)
    `Set.isSubsetOf` (domain r)

-- | Construct the /empty graph/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'Algebra.Graph.Relation.isEmpty'     empty == True
-- 'Algebra.Graph.Relation.hasVertex' x empty == False
-- 'Algebra.Graph.Relation.vertexCount' empty == 0
-- 'Algebra.Graph.Relation.edgeCount'   empty == 0
-- @
empty :: Relation a
empty = Relation Set.empty Set.empty

-- | Construct the graph comprising /a single isolated vertex/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'Algebra.Graph.Relation.isEmpty'     (vertex x) == False
-- 'Algebra.Graph.Relation.hasVertex' x (vertex x) == True
-- 'Algebra.Graph.Relation.hasVertex' 1 (vertex 2) == False
-- 'Algebra.Graph.Relation.vertexCount' (vertex x) == 1
-- 'Algebra.Graph.Relation.edgeCount'   (vertex x) == 0
-- @
vertex :: a -> Relation a
vertex x = Relation (Set.singleton x) Set.empty

-- | /Overlay/ two graphs. This is an idempotent, commutative and associative
-- operation with the identity 'empty'.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- 'Algebra.Graph.Relation.isEmpty'     (overlay x y) == 'Algebra.Graph.Relation.isEmpty'   x   && 'Algebra.Graph.Relation.isEmpty'   y
-- 'Algebra.Graph.Relation.hasVertex' z (overlay x y) == 'Algebra.Graph.Relation.hasVertex' z x || 'Algebra.Graph.Relation.hasVertex' z y
-- 'Algebra.Graph.Relation.vertexCount' (overlay x y) >= 'Algebra.Graph.Relation.vertexCount' x
-- 'Algebra.Graph.Relation.vertexCount' (overlay x y) <= 'Algebra.Graph.Relation.vertexCount' x + 'Algebra.Graph.Relation.vertexCount' y
-- 'Algebra.Graph.Relation.edgeCount'   (overlay x y) >= 'Algebra.Graph.Relation.edgeCount' x
-- 'Algebra.Graph.Relation.edgeCount'   (overlay x y) <= 'Algebra.Graph.Relation.edgeCount' x   + 'Algebra.Graph.Relation.edgeCount' y
-- 'Algebra.Graph.Relation.vertexCount' (overlay 1 2) == 2
-- 'Algebra.Graph.Relation.edgeCount'   (overlay 1 2) == 0
-- @
overlay :: Ord a => Relation a -> Relation a -> Relation a
overlay x y = Relation (domain x `union` domain y) (relation x `union` relation y)

-- | /Connect/ two graphs. This is an associative operation with the identity
-- 'empty', which distributes over the overlay and obeys the decomposition axiom.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory. Note that the
-- number of edges in the resulting graph is quadratic with respect to the number
-- of vertices of the arguments: /m = O(m1 + m2 + n1 * n2)/.
--
-- @
-- 'Algebra.Graph.Relation.isEmpty'     (connect x y) == 'Algebra.Graph.Relation.isEmpty'   x   && 'Algebra.Graph.Relation.isEmpty'   y
-- 'Algebra.Graph.Relation.hasVertex' z (connect x y) == 'Algebra.Graph.Relation.hasVertex' z x || 'Algebra.Graph.Relation.hasVertex' z y
-- 'Algebra.Graph.Relation.vertexCount' (connect x y) >= 'Algebra.Graph.Relation.vertexCount' x
-- 'Algebra.Graph.Relation.vertexCount' (connect x y) <= 'Algebra.Graph.Relation.vertexCount' x + 'Algebra.Graph.Relation.vertexCount' y
-- 'Algebra.Graph.Relation.edgeCount'   (connect x y) >= 'Algebra.Graph.Relation.edgeCount' x
-- 'Algebra.Graph.Relation.edgeCount'   (connect x y) >= 'Algebra.Graph.Relation.edgeCount' y
-- 'Algebra.Graph.Relation.edgeCount'   (connect x y) >= 'Algebra.Graph.Relation.vertexCount' x * 'Algebra.Graph.Relation.vertexCount' y
-- 'Algebra.Graph.Relation.edgeCount'   (connect x y) <= 'Algebra.Graph.Relation.vertexCount' x * 'Algebra.Graph.Relation.vertexCount' y + 'Algebra.Graph.Relation.edgeCount' x + 'Algebra.Graph.Relation.edgeCount' y
-- 'Algebra.Graph.Relation.vertexCount' (connect 1 2) == 2
-- 'Algebra.Graph.Relation.edgeCount'   (connect 1 2) == 1
-- @
connect :: Ord a => Relation a -> Relation a -> Relation a
connect x y = Relation (domain x `union` domain y) (relation x `union` relation y
    `union` (domain x >< domain y))

(><) :: Set a -> Set a -> Set (a, a)
x >< y = Set.fromDistinctAscList [ (a, b) | a <- Set.elems x, b <- Set.elems y ]

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L * log(L))/ time and /O(L)/ memory, where /L/ is the length
-- of the given list.
--
-- @
-- vertices []            == 'empty'
-- vertices [x]           == 'vertex' x
-- 'Algebra.Graph.Relation.hasVertex' x . vertices == 'elem' x
-- 'Algebra.Graph.Relation.vertexCount' . vertices == 'length' . 'Data.List.nub'
-- 'Algebra.Graph.Relation.vertexSet'   . vertices == Set.'Set.fromList'
-- @
vertices :: Ord a => [a] -> Relation a
vertices xs = Relation (Set.fromList xs) Set.empty

-- | Construct the graph from a list of edges.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edges []          == 'empty'
-- edges [(x,y)]     == 'Algebra.Graph.Relation.edge' x y
-- 'Algebra.Graph.Relation.edgeCount' . edges == 'length' . 'Data.List.nub'
-- @
edges :: Ord a => [(a, a)] -> Relation a
edges es = Relation (Set.fromList $ uncurry (++) $ unzip es) (Set.fromList es)

-- | Construct a graph from an adjacency list.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- fromAdjacencyList []                                  == 'empty'
-- fromAdjacencyList [(x, [])]                           == 'vertex' x
-- fromAdjacencyList [(x, [y])]                          == 'Algebra.Graph.Relation.edge' x y
-- 'overlay' (fromAdjacencyList xs) (fromAdjacencyList ys) == fromAdjacencyList (xs ++ ys)
-- @
fromAdjacencyList :: Ord a => [(a, [a])] -> Relation a
fromAdjacencyList as = Relation (Set.fromList vs) (Set.fromList es)
  where
    vs = concatMap (\(x, ys) -> x : ys) as
    es = [ (x, y) | (x, ys) <- as, y <- ys ]

-- | The sorted list of edges of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeList 'empty'          == []
-- edgeList ('vertex' x)     == []
-- edgeList ('Algebra.Graph.Relation.edge' x y)     == [(x,y)]
-- edgeList ('Algebra.Graph.Relation.star' 2 [1,3]) == [(2,1), (2,3)]
-- edgeList . 'edges'        == 'Data.List.nub' . 'Data.List.sort'
-- @
edgeList :: Ord a => Relation a -> [(a, a)]
edgeList = Set.toAscList . relation

-- | The /preset/ of an element @x@ is the set of elements that are related to
-- it on the /left/, i.e. @preset x == { a | aRx }@. In the context of directed
-- graphs, this corresponds to the set of /direct predecessors/ of vertex @x@.
-- Complexity: /O(n + m)/ time and /O(n)/ memory.
--
-- @
-- preset x 'empty'      == Set.empty
-- preset x ('vertex' x) == Set.empty
-- preset 1 ('Algebra.Graph.Relation.edge' 1 2) == Set.empty
-- preset y ('Algebra.Graph.Relation.edge' x y) == Set.fromList [x]
-- @
preset :: Ord a => a -> Relation a -> Set a
preset x = Set.mapMonotonic fst . Set.filter ((== x) . snd) . relation

-- | The /postset/ of an element @x@ is the set of elements that are related to
-- it on the /right/, i.e. @postset x == { a | xRa }@. In the context of directed
-- graphs, this corresponds to the set of /direct successors/ of vertex @x@.
-- Complexity: /O(n + m)/ time and /O(n)/ memory.
--
-- @
-- postset x 'empty'      == Set.empty
-- postset x ('vertex' x) == Set.empty
-- postset x ('Algebra.Graph.Relation.edge' x y) == Set.fromList [y]
-- postset 2 ('Algebra.Graph.Relation.edge' 1 2) == Set.empty
-- @
postset :: Ord a => a -> Relation a -> Set a
postset x = Set.mapMonotonic snd . Set.filter ((== x) . fst) . relation

-- | Remove a vertex from a given graph.
-- Complexity: /O(n + m)/ time.
--
-- @
-- removeVertex x ('vertex' x)       == 'empty'
-- removeVertex x . removeVertex x == removeVertex x
-- @
removeVertex :: Ord a => a -> Relation a -> Relation a
removeVertex x (Relation d r) = Relation (Set.delete x d) (Set.filter notx r)
  where
    notx (a, b) = a /= x && b /= x

-- | Remove an edge from a given graph.
-- Complexity: /O(log(m))/ time.
--
-- @
-- removeEdge x y ('AdjacencyMap.edge' x y)       == 'vertices' [x, y]
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge x y . 'removeVertex' x == 'removeVertex' x
-- removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2
-- removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2
-- @
removeEdge :: Ord a => a -> a -> Relation a -> Relation a
removeEdge x y (Relation d r) = Relation d (Set.delete (x, y) r)

-- | Transform a graph by applying a function to each of its vertices. This is
-- similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'Relation'.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- gmap f 'empty'      == 'empty'
-- gmap f ('vertex' x) == 'vertex' (f x)
-- gmap f ('Algebra.Graph.Relation.edge' x y) == 'Algebra.Graph.Relation.edge' (f x) (f y)
-- gmap id           == id
-- gmap f . gmap g   == gmap (f . g)
-- @
gmap :: (Ord a, Ord b) => (a -> b) -> Relation a -> Relation b
gmap f (Relation d r) = Relation (Set.map f d) (Set.map (\(x, y) -> (f x, f y)) r)

-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate.
-- Complexity: /O(m)/ time, assuming that the predicate takes /O(1)/ to
-- be evaluated.
--
-- @
-- induce (const True)  x      == x
-- induce (const False) x      == 'empty'
-- induce (/= x)               == 'removeVertex' x
-- induce p . induce q         == induce (\\x -> p x && q x)
-- 'Algebra.Graph.Relation.isSubgraphOf' (induce p x) x == True
-- @
induce :: Ord a => (a -> Bool) -> Relation a -> Relation a
induce p (Relation d r) = Relation (Set.filter p d) (Set.filter pp r)
  where
    pp (x, y) = p x && p y

-- | Compute the /reflexive closure/ of a 'Relation'.
-- Complexity: /O(n*log(m))/ time.
--
-- @
-- reflexiveClosure 'empty'      == 'empty'
-- reflexiveClosure ('vertex' x) == 'Algebra.Graph.Relation.edge' x x
-- @
reflexiveClosure :: Ord a => Relation a -> Relation a
reflexiveClosure (Relation d r) =
    Relation d $ r `union` Set.fromDistinctAscList [ (a, a) | a <- Set.elems d ]

-- | Compute the /symmetric closure/ of a 'Relation'.
-- Complexity: /O(m*log(m))/ time.
--
-- @
-- symmetricClosure 'empty'      == 'empty'
-- symmetricClosure ('vertex' x) == 'vertex' x
-- symmetricClosure ('Algebra.Graph.Relation.edge' x y) == 'Algebra.Graph.Relation.edges' [(x, y), (y, x)]
-- @
symmetricClosure :: Ord a => Relation a -> Relation a
symmetricClosure (Relation d r) = Relation d $ r `union` (Set.map swap r)

-- | Compute the /transitive closure/ of a 'Relation'.
-- Complexity: /O(n * m * log(m))/ time.
--
-- @
-- transitiveClosure 'empty'           == 'empty'
-- transitiveClosure ('vertex' x)      == 'vertex' x
-- transitiveClosure ('Algebra.Graph.Relation.path' $ 'Data.List.nub' xs) == 'Algebra.Graph.Relation.clique' ('Data.List.nub' xs)
-- @
transitiveClosure :: Ord a => Relation a -> Relation a
transitiveClosure old@(Relation d r)
    | r == newR = old
    | otherwise = transitiveClosure $ Relation d newR
  where
    newR = Set.unions $ r : [ preset x old >< postset x old | x <- Set.elems d ]

-- | Compute the /preorder closure/ of a 'Relation'.
-- Complexity: /O(n * m * log(m))/ time.
--
-- @
-- preorderClosure 'empty'           == 'empty'
-- preorderClosure ('vertex' x)      == 'Algebra.Graph.Relation.edge' x x
-- preorderClosure ('Algebra.Graph.Relation.path' $ 'Data.List.nub' xs) == 'reflexiveClosure' ('Algebra.Graph.Relation.clique' $ 'Data.List.nub' xs)
-- @
preorderClosure :: Ord a => Relation a -> Relation a
preorderClosure = reflexiveClosure . transitiveClosure

-- TODO: Optimise the implementation by caching the results of reflexive closure.
{-| The 'ReflexiveRelation' data type represents a /reflexive binary relation/
over a set of elements. Reflexive relations satisfy all laws of the
'C.Reflexive' type class and, in particular, the /self-loop/ axiom:

@'C.vertex' x == 'C.vertex' x * 'C.vertex' x@

The 'Show' instance produces reflexively closed expressions:

@show (1     :: ReflexiveRelation Int) == "edge 1 1"
show (1 * 2 :: ReflexiveRelation Int) == "edges [(1,1),(1,2),(2,2)]"@
-}
newtype ReflexiveRelation a = ReflexiveRelation { fromReflexive :: Relation a }
    deriving Num

instance Ord a => Eq (ReflexiveRelation a) where
    x == y = reflexiveClosure (fromReflexive x) == reflexiveClosure (fromReflexive y)

instance (Ord a, Show a) => Show (ReflexiveRelation a) where
    show = show . reflexiveClosure . fromReflexive

-- TODO: To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => C.Graph (ReflexiveRelation a) where
    type Vertex (ReflexiveRelation a) = a
    empty       = ReflexiveRelation empty
    vertex      = ReflexiveRelation . vertex
    overlay x y = ReflexiveRelation $ fromReflexive x `overlay` fromReflexive y
    connect x y = ReflexiveRelation $ fromReflexive x `connect` fromReflexive y

instance Ord a => C.Reflexive (ReflexiveRelation a)

-- TODO: Optimise the implementation by caching the results of symmetric closure.
{-|  The 'SymmetricRelation' data type represents a /symmetric binary relation/
over a set of elements. Symmetric relations satisfy all laws of the
'C.Undirected' type class and, in particular, the
commutativity of connect:

@'C.connect' x y == 'C.connect' y x@

The 'Show' instance produces symmetrically closed expressions:

@show (1     :: SymmetricRelation Int) == "vertex 1"
show (1 * 2 :: SymmetricRelation Int) == "edges [(1,2),(2,1)]"@
-}
newtype SymmetricRelation a = SymmetricRelation { fromSymmetric :: Relation a }
    deriving Num

instance Ord a => Eq (SymmetricRelation a) where
    x == y = symmetricClosure (fromSymmetric x) == symmetricClosure (fromSymmetric y)

instance (Ord a, Show a) => Show (SymmetricRelation a) where
    show = show . symmetricClosure . fromSymmetric

-- TODO: To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => C.Graph (SymmetricRelation a) where
    type Vertex (SymmetricRelation a) = a
    empty       = SymmetricRelation empty
    vertex      = SymmetricRelation . vertex
    overlay x y = SymmetricRelation $ fromSymmetric x `overlay` fromSymmetric y
    connect x y = SymmetricRelation $ fromSymmetric x `connect` fromSymmetric y

instance Ord a => C.Undirected (SymmetricRelation a)

-- TODO: Optimise the implementation by caching the results of transitive closure.
{-| The 'TransitiveRelation' data type represents a /transitive binary relation/
over a set of elements. Transitive relations satisfy all laws of the
'C.Transitive' type class and, in particular, the /closure/ axiom:

@y /= 'C.empty' ==> x * y + x * z + y * z == x * y + y * z@

For example, the following holds:

@'C.path' xs == 'C.clique' xs@

The 'Show' instance produces transitively closed expressions:

@show (1 * 2         :: TransitiveRelation Int) == "edge 1 2"
show (1 * 2 + 2 * 3 :: TransitiveRelation Int) == "edges [(1,2),(1,3),(2,3)]"@
-}
newtype TransitiveRelation a = TransitiveRelation { fromTransitive :: Relation a }
    deriving Num

instance Ord a => Eq (TransitiveRelation a) where
    x == y = transitiveClosure (fromTransitive x) == transitiveClosure (fromTransitive y)

instance (Ord a, Show a) => Show (TransitiveRelation a) where
    show = show . transitiveClosure . fromTransitive

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => C.Graph (TransitiveRelation a) where
    type Vertex (TransitiveRelation a) = a
    empty       = TransitiveRelation empty
    vertex      = TransitiveRelation . vertex
    overlay x y = TransitiveRelation $ fromTransitive x `overlay` fromTransitive y
    connect x y = TransitiveRelation $ fromTransitive x `connect` fromTransitive y

instance Ord a => C.Transitive (TransitiveRelation a)

-- TODO: Optimise the implementation by caching the results of preorder closure.
{-| The 'PreorderRelation' data type represents a binary relation over a set of
elements that is both transitive and reflexive. Preorders satisfy all laws of the
'Algebra.Graph.Class.Preorder' type class and, in particular, the /closure/
axiom:

@y /= 'C.empty' ==> x * y + x * z + y * z == x * y + y * z@

and the /self-loop/ axiom:

@'C.vertex' x == 'C.vertex' x * 'C.vertex' x@

For example, the following holds:

@'C.path' xs == 'C.clique' xs@

The 'Show' instance produces reflexively and transitively closed expressions:

@show (1             :: PreorderRelation Int) == "edge 1 1"
show (1 * 2         :: PreorderRelation Int) == "edges [(1,1),(1,2),(2,2)]"
show (1 * 2 + 2 * 3 :: PreorderRelation Int) == "edges [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]"@
-}
newtype PreorderRelation a = PreorderRelation { fromPreorder :: Relation a }
    deriving Num

instance (Ord a, Show a) => Show (PreorderRelation a) where
    show = show . preorderClosure . fromPreorder

instance Ord a => Eq (PreorderRelation a) where
    x == y = preorderClosure (fromPreorder x) == preorderClosure (fromPreorder y)

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => C.Graph (PreorderRelation a) where
    type Vertex (PreorderRelation a) = a
    empty       = PreorderRelation empty
    vertex      = PreorderRelation . vertex
    overlay x y = PreorderRelation $ fromPreorder x `overlay` fromPreorder y
    connect x y = PreorderRelation $ fromPreorder x `connect` fromPreorder y

instance Ord a => C.Reflexive  (PreorderRelation a)
instance Ord a => C.Transitive (PreorderRelation a)
instance Ord a => C.Preorder   (PreorderRelation a)
