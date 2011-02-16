{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}
module Rules where

import Prelude.Unicode
import Graph
import GraphRewriting.Rule as Rule
import GraphRewriting.Pattern
import GraphRewriting.Graph.Read
import Control.Applicative
import Data.Monoid.Unicode


compileShare ∷ (View [Port] n, View NodeWW n) ⇒ Rule n
compileShare = do
	Multiplexer {out = o, ins = is} ← node
	case is of
		[ ] → replace $ byNode Eraser {inp = o}
		[i] → rewire [[o,i]]
		ins → let (ins1, ins2) = splitAt (length ins `div` 2) ins in replace $ do
			(o1,o2) ← (,) <$> byEdge <*> byEdge
			byNode Duplicator {active = False, inp = o, out1 = o1, out2 = o2}
			byNode Multiplexer {out = o1, ins = ins1}
			byNode Multiplexer {out = o2, ins = ins2}

beta ∷ (View [Port] n, View NodeWW n) ⇒ Rule n
beta = do
	abs@Abstractor {} ← node
	app@Applicator {} ← nodeWith $ inp abs
	require (inp abs ≡ func app)
	rewire [[inp app, body abs], [arg app, var abs]]

initDuplication ∷ (View [Port] n, View NodeWW n) ⇒ Rule n
initDuplication = do
	dup@Duplicator {active = False} ← node
	abs@Abstractor {} ← nodeWith $ inp dup
	require $ inp abs ≡ inp dup
	replace $ do
		(b1,b2,v1,v2) ← (,,,) <$> byEdge <*> byEdge <*> byEdge <*> byEdge
		byNode abs {inp = out1 dup, body = b1, var = v1}
		byNode abs {inp = out2 dup, body = b2, var = v2}
		byNode Duplicator {out1 = b1, out2 = b2, inp = body abs, active = True}
		byNode Duplicator {out1 = v1, out2 = v2, inp = var abs,  active = True}

duplicateAbstractor ∷ (View [Port] n, View NodeWW n) ⇒ Rule n
duplicateAbstractor = do
	dup@Duplicator {active = True} ← node
	abs@Abstractor {} ← nodeWith $ inp dup
	require $ body abs ≡ inp dup
	replace $ do
		(i1,i2,v1,v2) ← (,,,) <$> byEdge <*> byEdge <*> byEdge <*> byEdge
		byNode dup {inp = inp abs, out1 = i1, out2 = i2}
		byNode abs {inp = i1, body = out1 dup, var = v1}
		byNode abs {inp = i2, body = out2 dup, var = v2}
		byNode dup {out1 = v1, out2 = v2, inp = var abs}

duplicateApplicator ∷ (View [Port] n, View NodeWW n) ⇒ Rule n
duplicateApplicator = do
	dup@Duplicator {active = True} ← node
	app@Applicator {} ← nodeWith $ inp dup
	require $ inp dup ∈ [func app, arg app]
	if inp dup ≡ func app
		then do
			require $ func app ≡ inp dup
			replace $ do
				(i1,i2,a1,a2) ← (,,,) <$> byEdge <*> byEdge <*> byEdge <*> byEdge 
				byNode dup {inp = inp app, out1 = i1, out2 = i2}
				byNode Applicator {inp = i1, func = out1 dup, arg = a1}
				byNode Applicator {inp = i2, func = out2 dup, arg = a2}
				byNode dup {out1 = a1, out2 = a2, inp = arg app}
		else do
			require $ arg app ≡ inp dup
			replace $ do
				(i1,i2,a1,a2) ← (,,,) <$> byEdge <*> byEdge <*> byEdge <*> byEdge
				byNode dup {inp = inp app, out1 = i1, out2 = i2}
	 	 	 	byNode Applicator {inp = i1, func = a1, arg = out1 dup}
	 	 	 	byNode Applicator {inp = i2, func = a2, arg = out2 dup}
	 	 	 	byNode dup {out1 = a1, out2 = a2, inp = func app}

duplicateDuplicator ∷ (View [Port] n, View NodeWW n) ⇒ Rule n
duplicateDuplicator = linear $ do
	dup1@Duplicator {active = True} ← node
	dup2@Duplicator {active = False} ← nodeWith $ inp dup1
	require $ inp dup1 ≡ inp dup2
	replace $ do
		(ll,lr,rl,rr) ← (,,,) <$> byEdge <*> byEdge <*> byEdge <*> byEdge
		byNode dup1 {inp = out1 dup2, out1 = ll, out2 = lr}
		byNode dup1 {inp = out2 dup2, out1 = rl, out2 = rr}
		byNode dup2 {inp = out1 dup1, out1 = ll, out2 = rl}
		byNode dup2 {inp = out2 dup1, out1 = lr, out2 = rr}

duplicatePrimitive ∷ (View [Port] n, View NodeWW n) ⇒ Rule n
duplicatePrimitive = do
	d@Duplicator {} ← node
	p@Primitive {} ← nodeWith $ inp d
	replace $ byNode p {inp = out1 d} ⊕ byNode p {inp = out2 d}

duplicateEraser ∷ (View [Port] n, View NodeWW n) ⇒ Rule n
duplicateEraser = do
	d@Duplicator {} ← node
	e@Eraser {} ← nodeWith $ inp d
	replace $ byNode Eraser {inp = out1 d} ⊕ byNode Eraser {inp = out2 d}

annihilate ∷ (View [Port] n, View NodeWW n) ⇒ Rule n
annihilate = linear $ do
	dup1@Duplicator {active = True} ← node
	dup2@Duplicator {active = True} ← nodeWith $ inp dup1
	require $ inp dup1 ≡ inp dup2
	rewire [[out1 dup1, out1 dup2], [out2 dup1, out2 dup2]]

deactivate ∷ (View [Port] n, View NodeWW n) ⇒ Rule n
deactivate = do
	dup@Duplicator {active = True} ← node
	replace $ byNode dup {active = False}

eliminate ∷ (View [Port] n, View NodeWW n) ⇒ Rule n
eliminate = do
	e@Eraser {} ← node
	d@Duplicator {out1 = o1, out2 = o2} ← neighbour =<< previous
	require $ inp e ∈ [o1, o2]
	if inp e ≡ o1
		then rewire [[inp d, o2]]
		else rewire [[inp d, o1]]

erase ∷ (View [Port] n, View NodeWW n) ⇒ Rule n
erase = linear $ do
	e@Eraser {} ← node
	let
		abstractor = do
			a@Abstractor {} ← nodeWith $ inp e
			require $ inp a ≡ inp e
			replace $ byNode Eraser {inp = body a} ⊕ byNode Eraser {inp = var a}
		applicator = do
			a@Applicator {} ← nodeWith $ inp e
			require $ inp a ≡ inp e
			replace $ byNode Eraser {inp = func a} ⊕ byNode Eraser {inp = arg a}
		eraser = do
			Eraser {} ← nodeWith $ inp e
			Rule.erase
	abstractor <|> applicator <|> eraser

applyPrimitive ∷ (View [Port] n, View NodeWW n) ⇒ Rule n
applyPrimitive = linear $ do
	a@Applicator {} ← node
	f@Primitive {} ← nodeWith $ func a
	x@Primitive {} ← nodeWith $ arg a
	replace $ byNode Primitive {name = "(" ⧺ name f ⧺ " " ⧺ name x ⧺ ")", inp = inp a}
