{-# LANGUAGE UnicodeSyntax, FlexibleInstances, MultiParamTypeClasses #-}
module Graph where

import Data.View
import GraphRewriting.Graph
import GraphRewriting.Graph.Write
import Term
import Data.Maybe (listToMaybe)

data Vertex
	= Applicator {inp, out1, out2 ∷ Port}
	| Variable   {inp ∷ Port, name ∷ Char}
	| Root       {out ∷ Port}

instance View [Port] Vertex where
	inspect node = case node of
		Applicator {inp = i, out1 = o1, out2 = o2} → [i,o1,o2]
		Variable   {inp = i}                       → [i]
		Root       {out = o}                       → [o]
	update ports node = case node of
		Applicator {} → node {inp = i, out1 = o1, out2 = o2} where [i,o1,o2] = ports
		Variable   {} → node {inp = i}                       where [i]       = ports
		Root       {} → node {out = o}                       where [o]       = ports

fromTerm ∷ Term → Graph Vertex
fromTerm term = flip execGraph emptyGraph $ do
	e ← compile term
	newNode Root {out = e}

compile ∷ Term → Rewrite Vertex Edge
compile term = do
	e ← newEdge
	_ ← case term of
		Var v → newNode Variable {inp = e, name = v}
		App f x → do
			ef ← compile f
			ex ← compile x
			newNode Applicator {inp = e, out1 = ef, out2 = ex}
	return e

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
