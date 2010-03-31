{-# LANGUAGE UnicodeSyntax, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module GraphRewriting.Graph.Internal where

import Prelude.Unicode
import Control.Monad.State
import Data.IntMap as Map (IntMap, lookup)
import Data.IntSet (IntSet)
import Control.Monad.Reader


-- | Hypergraph that holds nodes of type @n@. Nodes can be referenced by type 'Node', edges by type 'Edge', see "GraphRewriting.Graph.Read" and "GraphRewriting.Graph.Write"
data Graph n = Graph {nodeMap ∷ IntMap n, edgeMap ∷ IntMap IntSet, nextKey ∷ Int}

newtype Rewrite n a = Rewrite {rewrite ∷ State (Graph n) a}
	deriving (MonadState (Graph n), Monad, Functor, MonadFix)

deriving instance Applicative (Rewrite n)

newtype Node = Node {nKey ∷ Int} deriving (Eq, Ord) -- TODO: change this into Integer to avert overflow
newtype Port = Edge {eKey ∷ Int} deriving (Eq, Ord) -- TODO: change this into Integer to avert overflow
type Edge = Port -- ^ a hyperedge really, connecting a non-empty subset of the graph's nodes (see 'attachedNodes')

instance Show Node where show = show . nKey
instance Show Edge where show = show . eKey

instance MonadReader (Graph n) (Rewrite n) where
	ask = Rewrite get
	local f m = Rewrite $ gets (evalState (rewrite m) . f)

readRef ∷ Monad m ⇒ Int → IntMap a → m a
readRef key = maybe (fail "readRef: referentiation failed") return . Map.lookup key

readEdge ∷ MonadReader (Graph n) r ⇒ Edge → r IntSet
readEdge (Edge e) = maybe (fail $ "readEdge: edge with ID " ⧺ show e ⧺ " does not exist") return . readRef e =<< asks edgeMap

modifyNodeMap ∷ (IntMap n → IntMap n) → Rewrite n ()
modifyNodeMap f = modify $ \g → g {nodeMap = f $ nodeMap g}

modifyEdgeMap ∷ (IntMap IntSet → IntMap IntSet) → Rewrite n ()
modifyEdgeMap f = modify $ \g → g {edgeMap = f $ edgeMap g}

-- | allocate and reserve a new ref
newRef ∷ Rewrite n Int
newRef = do
	i ← gets nextKey
	modify $ \g → g {nextKey = i + 1}
	return i

-- | Hand out an infinite number of fresh refs, without reserving them (obviously).
freeRefs ∷ MonadReader (Graph n) r ⇒ r [Int]
freeRefs = enumFrom `liftM` asks nextKey

reserveRefs ∷ [Int] → Rewrite n ()
reserveRefs refs = modify $ \g → g {nextKey = maximum refs}
