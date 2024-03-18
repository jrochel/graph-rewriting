{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}
-- Now we are going to specify rewriting rules that implement the SKI combinator reductions. Refer to the module decumentation of GraphRewriting.Pattern and GraphRewriting.Rule for more detailed information about the functions used here.
module INet.Rules where

import Prelude.Unicode
import Data.Monoid.Unicode
import Control.Applicative
import INet.Graph
import GraphRewriting
import GraphRewriting.Pattern.InteractionNet -- This gives us the 'activePair' pattern.


-- The simplest of the SKI combinators is the I combinator. We match on the active pair of an I node and an applicator using the 'activePair' function which resides in the 'Pattern' monad. Note that the :-: is an infix constructor and is just an alternative representation of a pair. With the left-hand side of the rule given, we build a rule out of it that erases the matched nodes and connects the edges at the input port and the right output port of the applicator using the 'rewire' function.
ruleI ∷ (View [Port] n, View SKI n) ⇒ Rule n
ruleI = do
	I {} :-: A {inp = iA, out2 = o2} ← activePair
	rewire [[iA,o2]]

-- The K0 node represents a K combinator that has not yet accumulated an argument, which is what this rule does. Again, we match an active pair of a K0 node and an applicator. Then we replace these nodes by a K1 node that has the right-hand subgraph of the applicator as an argument (at its output port).
ruleK0 ∷ (View [Port] n, View SKI n) ⇒ Rule n
ruleK0 = do
	K0 {} :-: A  {inp = iA, out2 = o2} ← activePair
	replace $ byNode K1 {inp = iA, out = o2}

-- The 'replace*' functions can be used replace the matched nodes by a combination of new nodes and rewirings, hence the constructors 'Wire' and 'Node'.
ruleK1 ∷ (View [Port] n, View SKI n) ⇒ Rule n
ruleK1 = do
	K1 {out = oK} :-: A {inp = iA, out2 = o2A} ← activePair
	replace $ byWire iA oK ⊕ byNode E {inp = o2A}

ruleS0 ∷ (View [Port] n, View SKI n) ⇒ Rule n
ruleS0 = do
	S0 {} :-: A {inp = iA, out2 = o2A} ← activePair
	replace $ byNode S1 {inp = iA, out = o2A}

ruleS1 ∷ (View [Port] n, View SKI n) ⇒ Rule n
ruleS1 = do
	S1 {out = oS} :-: A {inp = iA, out2 = o2A} ← activePair
	replace $ byNode S2 {inp = iA, out1 = oS, out2 = o2A}

-- If we need new edges for the right-hand side of the rewrite rule you can use 'replaceN' with N > 0.
ruleS2 ∷ (View [Port] n, View SKI n) ⇒ Rule n
ruleS2 = do
	S2 {inp = iS, out1 = oS1, out2 = o2S} :-: a@A {out1 = o1A, out2 = o2A} ← activePair
	replace $ do
		(i1D,iB,i2D) ← (,,) <$> byEdge <*> byEdge <*> byEdge
		byNode A {inp = iS, out1 = oS1, out2 = i1D}
		byNode a {out2 = iB}
		byNode D {inp1 = i1D, inp2 = i2D, out = o2A}
		byNode A {inp = iB, out1 = o2S, out2 = i2D}

-- This is an abstraction to match any active pair that involves a node with arity 0.
arity0 ∷ (View [Port] n, View SKI n) ⇒ Pattern n (Pair SKI)
arity0 = i <|> k <|> s where
	i = do {pair@(n :-: I  {}) ← activePair; return pair}
	k = do {pair@(n :-: K0 {}) ← activePair; return pair}
	s = do {pair@(n :-: S0 {}) ← activePair; return pair}

arity1 ∷ (View [Port] n, View SKI n) ⇒ Pattern n (Pair SKI)
arity1 = k <|> s where
	k = do {pair@(n :-: K1 {}) ← activePair; return pair}
	s = do {pair@(n :-: S1 {}) ← activePair; return pair}

-- If the left-hand side is to be erased completely without any rewirings or new nodes to be replaced with, use the 'erase'.
ruleE0 ∷ (View [Port] n, View SKI n) ⇒ Rule n 
ruleE0 = do
	E {inp = iE} :-: n ← arity0
	erase
	
ruleE1 ∷ (View [Port] n, View SKI n) ⇒ Rule n 
ruleE1 = do
	E {} :-: n ← arity1
	replace $ byNode E {inp = out n}
	
ruleE2 ∷ (View [Port] n, View SKI n) ⇒ Rule n 
ruleE2 = do
	E {inp = iE} :-: S2 {inp = iS, out1 = o1, out2 = o2} ← activePair
	replace $ byNode E {inp = o1} ⊕ byNode E {inp = o2}

ruleD0  ∷ (View [Port] n, View SKI n) ⇒ Rule n
ruleD0 = do
	D {inp1 = iD1, inp2 = iD2, out = oD} :-: n ← arity0
	replace $ byNode n {inp = iD1} ⊕ byNode n {inp = iD2}

ruleD1  ∷ (View [Port] n, View SKI n) ⇒ Rule n
ruleD1 = do
	D {inp1 = iD1, inp2 = iD2, out = oD} :-: n ← arity1
	replace $ do
		(iD1',iD2') ← (,) <$> byEdge <*> byEdge
		byNode n {inp = iD1, out = iD1'}
		byNode n {inp = iD2, out = iD2'}
		byNode D {inp1 = iD1', inp2 = iD2', out = out n}
	
ruleD2 ∷ (View [Port] n, View SKI n) ⇒ Rule n 
ruleD2 = do
	D {inp1 = iD1, inp2 = iD2, out = oD} :-: S2 {inp = iS, out1 = o1, out2 = o2} ← activePair
	replace $ do
		(l1,l2,x1,x2) ← (,,,) <$> byEdge <*> byEdge <*> byEdge <*> byEdge
		byNode S2 {inp = iD1, out1 = l1, out2 = x1}
		byNode S2 {inp = iD2, out1 = x2, out2 = l2}
		byNode D {inp1 = l1, inp2 = x2, out = o1}
		byNode D {inp1 = x1, inp2 = l2, out = o2}

-- Here is the only rule that is not an interaction-net reduction, hence it does not rely on the 'activePair' pattern. First we match on an eraser node anywhere in the graph. Next we require a duplicator node that is connected to the eraser. Therefore we use the 'previous' pattern that returns a reference to the previously matched node and feed it to the 'neighbour' function that matches on nodes connected to the referenced node.
eliminate ∷ (View [Port] n, View SKI n) ⇒ Rule n
eliminate = do
	E {inp = iE} ← node
	D {out = oD, inp1 = i1, inp2 = i2} ← nodeWith iE
	require (iE ≡ i1 ∨ iE ≡ i2)
	if iE ≡ i1
		then rewire [[oD,i2]]
		else rewire [[oD,i1]]

-- Together with the signature defined in Graph.hs these rules can be used to actually perform graph transformations. Therefore use the 'GraphRewriting.Graph.execGraph' and 'GraphRewriting.Rule.apply' functions. In INet/GL.hs we're going to provide the layouting and graphics engines information on how to arrange and display the nodes.
