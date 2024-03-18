{-# LANGUAGE UnicodeSyntax, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, OverlappingInstances #-}
module GraphRewriting.Strategies.Control where

import Data.View
import GraphRewriting.Graph
--import GraphRewriting.Layout.PortSpec
import qualified Data.IntMap as Map

data Wrapper n = Wrapper {control :: Control, wrapped :: n}

data Control = Control {stack ∷ [Node]} | NoControl

instance View Control (Wrapper n) where
	inspect = control
	update c n = n {control = c}

instance View v n ⇒ View v (Wrapper n) where
	inspect = inspect . wrapped
	update v n = n {wrapped = update v $ wrapped n}

-- | Wraps the nodes of a graph, augmenting them with control information
wrapGraph :: Graph n -> Graph (Wrapper n)
wrapGraph graph = graph {nodeMap = newNodeMap} where
		wrapNode n = Wrapper {control = NoControl, wrapped = n}
		rootNodeId = minimum (Map.keys $ nodeMap graph)
		controlgraph = unsafeMapNodes wrapNode graph
 		newNodeMap = Map.update (\x → Just $ x {control = Control []}) rootNodeId (nodeMap controlgraph)
