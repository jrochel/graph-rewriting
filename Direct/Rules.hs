{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}
module Direct.Rules where

import Prelude.Unicode
import Data.Monoid.Unicode
import Direct.Graph
import GraphRewriting
import Control.Applicative


-- <chunk: rules>
ruleS ∷ (View [Port] n, View SKI n) ⇒ Rule n 
ruleS = do
	S {inp = si} ← node
	Applicator {inp = i1, out1 = o1, out2 = f} ← nodeWith si
	require (si ≡ o1)
	Applicator {inp = i2, out1 = o2, out2 = g} ← nodeWith i1
	require (i1 ≡ o2)
	Applicator {inp = i3, out1 = o3, out2 = x} ← nodeWith i2
	require (i2 ≡ o3)
	replace $ do
		(l,r,lr,rr) ← (,,,) <$> byEdge <*> byEdge <*> byEdge <*> byEdge
		byNode Applicator {inp = i3, out1 = l, out2 = r}
		byNode Applicator {inp = l, out1 = f, out2 = lr}
		byNode Applicator {inp = r, out1 = g, out2 = rr}
		byNode Duplicator {inp1 = lr, inp2 = rr, out = x}

-- <chunk: ruleK>
ruleK ∷ (View [Port] n, View SKI n) ⇒ Rule n 
ruleK = do
	K {inp = si} ← node
	Applicator {inp = i1, out1 = o1, out2 = x} ← nodeWith si
	require (si ≡ o1)
	Applicator {inp = i2, out1 = o2, out2 = y} ← nodeWith i1
	require (i1 ≡ o2)
	replace $ byWire x i2 ⊕ byNode Eraser {inp = y}
-- </chunk: ruleK>

ruleI ∷ (View [Port] n, View SKI n) ⇒ Rule n
ruleI = do
	I {inp = iI} ← node
	Applicator {inp = iA, out1 = o1, out2 = o2} ← nodeWith iI
	require (iI ≡ o1)
	rewire [[iA,o2]]

combinatorAt ∷ (View [Port] n, View SKI n) ⇒ Edge → Pattern n SKI
combinatorAt e = anyOf [s,k,i] where
	s = do {s@S {} ← nodeWith e; return s}
	k = do {k@K {} ← nodeWith e; return k}
	i = do {i@I  {} ← nodeWith e; return i}

duplicateCombinator  ∷ (View [Port] n, View SKI n) ⇒ Rule n
duplicateCombinator = do
	Duplicator {inp1 = i1, inp2 = i2, out = o} ← node
	c ← combinatorAt o
	replace $ byNode c {inp = i1} ⊕ byNode c {inp = i2}

duplicateApp ∷ (View [Port] n, View SKI n) ⇒ Rule n 
duplicateApp = do
	Duplicator {inp1 = i1, inp2 = i2, out = o} ← node
	Applicator {inp = i, out1 = o1, out2 = o2} ← nodeWith o
	replace $ do
		(l,lr,rl,r) ← (,,,) <$> byEdge <*> byEdge <*> byEdge <*> byEdge
		byNode Applicator {inp = i1, out1 = l, out2 = lr}
		byNode Applicator {inp = i2, out1 = rl, out2 = r}
		byNode Duplicator {inp1 = l, inp2 = rl, out = o1}
		byNode Duplicator {inp1 = lr, inp2 = r, out = o2}

eraseCombinator ∷ (View [Port] n, View SKI n) ⇒ Rule n 
eraseCombinator = do
	Eraser {inp = i} ← node
	_ ← combinatorAt i
	erase

eraseApp ∷ (View [Port] n, View SKI n) ⇒ Rule n 
eraseApp = do
	Eraser {inp = i} ← node
	Applicator {out1 = o1, out2 = o2} ← nodeWith i
	replace $ byNode Eraser {inp = o1} ⊕ byNode Eraser {inp = o2}

eliminate ∷ (View [Port] n, View SKI n) ⇒ Rule n
eliminate = do
	Eraser {inp = iE} ← node
	Duplicator {out = oD, inp1 = i1, inp2 = i2} ← nodeWith iE
	require (iE ≡ i1 ∨ iE ≡ i2)
	if iE ≡ i1
		then rewire [[oD,i2]]
		else rewire [[oD,i1]]
-- <chunk: rules>
