{-# LANGUAGE UnicodeSyntax, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module GraphRewriting.Strategies.Control where

import Data.View
import GraphRewriting.Graph
--import GraphRewriting.Layout.PortSpec
import qualified Data.IntMap as Map

data ControlWrapper n = ControlWrapper {control :: Control, wrapped :: n}

data Control = Control {stack ∷ [Node]} | NoControl

instance View Control (ControlWrapper n) where
	inspect = control
	update c n = n {control = c}

instance View v n ⇒ View v (ControlWrapper n) where
	inspect = inspect . wrapped
	update v n = n {wrapped = update v $ wrapped n}

--instance PortSpec n => PortSpec (ControlWrapper n) where
--	portSpec = portSpec . wrapped

-- | Wraps the nodes of a graph, augmenting them with control information
wrapGraph :: Graph n -> Graph (ControlWrapper n)
wrapGraph graph = graph {nodeMap = newNodeMap} where
		wrapNode k n = ControlWrapper {control = NoControl, wrapped = n}
		rootNodeId = minimum (Map.keys $ nodeMap graph)
 		controlgraph = unsafeMapNodesUnique wrapNode graph
 		newNodeMap = Map.update (\x → Just $ x {control = Control []}) rootNodeId (nodeMap controlgraph)
