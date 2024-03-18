{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
-- To understand how to use the graph-rewriting library this module is a good point to start reading. Some fundamentals are covered in the documentation of the "GraphRewriting" module.
module INet.Graph where

import Data.View -- The 'View' abstraction will appear throughout the modules, see "GraphRewriting" and "Data.View".
import GraphRewriting.Graph -- This gives us basic data types like 'Port' or 'Edge'.
import GraphRewriting.Graph.Write -- We need graph modification functions in order to build a graph from the term.
-- Most of the rewriting rules we specify are interaction net reductions, for which a convenient pattern is predefined.
import GraphRewriting.Pattern.InteractionNet
-- This module specifies the term representation of an SKI expression (abstract syntax tree) 
import qualified Common.Term as Term
import Control.Monad (void)


-- The signature of the graph is determined by the node type we provide. For each node constructor we define as record fields a fixed collection of ports. Here we name ports attached at the top of the nodes input ports and nodes at the bottom output ports.
data SKI
	= R {out ∷ Port} -- Root node, which is supposed to occur exactly once in the graph and correpsonds to the root of the term
	| A {inp, out1, out2 ∷ Port} -- An applicator. It's left-hand subgraph (out1) denotes the expression to which the expression represented by the right-hand subgraph is applied.
	| I {inp ∷ Port} -- The identity combinator.
	| E {inp ∷ Port} -- An eraser. This node type is used to delete the subgraph discarded by the K combinator.
	| D {inp1, inp2, out ∷ Port} -- A duplicator is used to implement sharing in the SKI combinator calculus
	| V {inp ∷ Port, name ∷ String} -- A free variable
	-- Here we implement the SKI combinators in a very fine-grained manner, namely a combinator has to accumulate its arguments one-by-one before it can be applied. That is why we have two variants of the K combinator: K0 (no arguments accumulated) and K1 (saturated).
	| K0 {inp ∷ Port}
	| K1 {inp ∷ Port, out ∷ Port}
	-- The same goes for the S combinator, which takes even one more parameter.
	| S0 {inp ∷ Port}
	| S1 {inp ∷ Port, out ∷ Port}
	| S2 {inp ∷ Port, out1 ∷ Port, out2 ∷ Port}

-- While it is very convenient to specify the nodes' ports as record fields as above it does not reveal the graph structure to the library. Therefore we have to provide some boilerplate code to expose the ports, for which we use the 'View' abstraction. In the future some Template Haskell might be included in the library to avoid this effort.
instance View [Port] SKI where
	-- For each node type we simply have to return a list containing all the ports.
	inspect ski = case ski of
		R {out = o}                        → [o]
		A {inp = i, out1 = o1, out2 = o2}  → [i,o1,o2]
		I {inp = i}                        → [i]
		E {inp = i}                        → [i]
		D {inp1 = i1, inp2 = i2, out = o}  → [i1,i2,o]
		V {inp = i}                        → [i]
		K0 {inp = i}                       → [i]
		K1 {inp = i, out = o}              → [i,o]
		S0 {inp = i}                       → [i]
		S1 {inp = i, out = o}              → [i,o]
		S2 {inp = i, out1 = o1, out2 = o2} → [i,o1,o2]
	-- But we also need to provide means for the library to update these ports.
	update ports ski = case ski of
		R  {} → ski {out = o}                       where [o]       = ports
		A  {} → ski {inp = i, out1 = o1, out2 = o2} where [i,o1,o2] = ports
		I  {} → ski {inp = i}                       where [i]       = ports
		E  {} → ski {inp = i}                       where [i]       = ports
		D  {} → ski {inp1 = i1, inp2 = i2, out = o} where [i1,i2,o] = ports
		V  {} → ski {inp = i}                       where [i]       = ports
		K0 {} → ski {inp = i}                       where [i]       = ports
		K1 {} → ski {inp = i, out = o}              where [i,o]     = ports
		S0 {} → ski {inp = i}                       where [i]       = ports
		S1 {} → ski {inp = i, out = o}              where [i,o]     = ports
		S2 {} → ski {inp = i, out1 = o1, out2 = o2} where [i,o1,o2] = ports

-- Since we want to make use of interaction net reductions (using the 'activePair' pattern) we need to specify the principal port for each node type in the form of an index into the port list above.
instance View SKI n ⇒ INet n where
	principalPort n = case inspect n of
		R {out = o}                        → o
		A {inp = i, out1 = o1, out2 = o2}  → o1
		I {inp = i}                        → i
		E {inp = i}                        → i
		D {inp1 = i1, inp2 = i2, out = o}  → o
		V {inp = i}                        → i
		K0 {inp = i}                       → i
		K1 {inp = i, out = o}              → i
		S0 {inp = i}                       → i
		S1 {inp = i, out = o}              → i
		S2 {inp = i, out1 = o1, out2 = o2} → i

-- In "Term" a little SKI term parser is given. The code below implements a small compiler that translates the abstract syntax tree into a graph. Here you can see how primitive graph transformation functions like 'newNode' and 'newEdge' can be used to build a graph inside the 'GraphRewriting.Graph.Rewrite' monad. Also it shows how an edge can be attached to a node's port, simply by assigning it to the corresponding record field.
fromTerm ∷ Term.Expr → Graph SKI
fromTerm term = flip execGraph emptyGraph $ do
	e ← compile term
	newNode R {out = e}

compile ∷ Term.Expr → Rewrite SKI Edge
compile term = do
	e ← newEdge
	void $ case term of
		Term.A f x → do
			ef ← compile f
			ex ← compile x
			newNode A {inp = e, out1 = ef, out2 = ex}
		Term.S → newNode S0 {inp = e}
		Term.K → newNode K0 {inp = e}
		Term.I → newNode I {inp = e}
		Term.V v → newNode V {inp = e, name = v}
	return e

-- Continue reading in the "INet.Rules" module.
