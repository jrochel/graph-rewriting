{-# LANGUAGE UnicodeSyntax, FlexibleContexts, ScopedTypeVariables #-}
module Rules where

import Prelude.Unicode
import Graph
import GraphRewriting


unshare ∷ (View [Port] n, View Vertex n) ⇒ Rule n
unshare = do
	e ← edge
	c ← liftReader $ edgeCardinality e
	require (c > 2)
	v ← nodeWith e
	require (e ≡ inp v)
	rewrite $ \[n] → do
		es ← deleteEdge e
		v' ← inspectNode n
		mapM_ (copyNode n) [v {inp = i} | i ← es, i ≢ inp v']
		deleteNode n

eraseRule ∷ (View [Port] n, View Vertex n) ⇒ Rule n
eraseRule = do
	e ← edge
	requireM $ liftReader $ dangling e
	_ ∷ [Port] ← nodeWith e
	erase
