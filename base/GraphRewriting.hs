-- | This is a monadic graph rewriting library for port graphs with a strong emphasis on nodes. It aims primarily at making it as convenient as possible to specify graph rewriting systems in Haskell and to experiment with them. There are a few aspects of the design to be pointed out:
--
-- 1. The port graph is actually a hypergraph, therefore if we speak of edges we are in fact referring to hyperedges.
--
-- 2. The graph structure is essentially representated as a collection of nodes. The nodes have a user-defined type, where each node features a list of 'Port's to each of which an 'Edge' is attached. Edges are unlabeled and can not exist autonomously, i.e. each edge is connected to at least one port. Each port is connetected to exactly one edge. Two ports are connected if (and only if) they share the same edge. What is particularly convenient is how these ports can be modeled as constructor fields of a user-defined node type.
--
-- 3. An important abstraction used in this library is the multi-parameter type-class 'View'. It permits to expose a certain aspect of a node, allowing both to 'inspect' or 'update' it, while hiding the internal representation of the node. By that it is easy to specify the rewrite system in a way, that it can not only be applied to a graph with nodes of a fixed node type, but also to a 'Graph' with polymorphic node type @n@. The nodes merely have to /expose/ values of type @v@ by means of defining a @View v@ on @n@. The 'View' abstraction is also used to expose the nodes' ports (and therefore the graph structure) to this library.
--
-- 4. Rewrite 'Rule' are represented as 'Pattern's that return a 'Rewrite'. A 'Pattern' is a branching scrutinisation of the graph that returns a result for every possible matching position in the graph. A 'Rule' is essentially a 'Pattern' that returns a 'Rewrite'. A 'Rewrite' is a monadic modification of the graph structure. In a 'Rule' the 'Rewrite' part can conveniently use the variables bound in the 'Pattern' code.
--
-- See the graph-rewriting-ski package for an example of a simple rewrite system. Together with the graph-rewriting-layout and the graph-rewriting-gl packages it is easy to build a graphical and interactive application to experiment with your rewrite system.
--
-- This does library does not offer combinators for defining strategies. These are offered by the package graph-rewriting-strategies.
module GraphRewriting
	(module GraphRewriting.Graph.Types,
	 -- | graph representation, 'Rewrite' monad
	 module GraphRewriting.Graph,
	 -- | mapping over nodes, graph creation, applying a 'Rewrite'
	 module GraphRewriting.Graph.Read,
	 -- | monadic graph scrutinisation
	 module GraphRewriting.Graph.Write,
	 -- | monadic graph modification
	 module GraphRewriting.Pattern,
	 -- | branching graph scrutinisation that keeps track of scrutinised nodes
	 module GraphRewriting.Rule,
	 -- | combine a 'Pattern' with a 'Rewrite' to obtain a rewrite rule
	 module Data.View)
where

import GraphRewriting.Graph
import GraphRewriting.Graph.Types
import GraphRewriting.Graph.Read
import GraphRewriting.Graph.Write
import GraphRewriting.Pattern
import GraphRewriting.Rule
import Data.View
