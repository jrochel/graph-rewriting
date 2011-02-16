{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}
module Rules where

import Prelude.Unicode
import Graph
import GraphRewriting.Rule
import GraphRewriting.Pattern
import GraphRewriting.Pattern.InteractionNet
import GraphRewriting.Graph.Read
import Data.List (transpose)


compileShare ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
compileShare = do
	Multiplier {out = o, ins = is} ← node
	case is of
		[ ] → replace0 [Node $ Eraser {inp = o}]
		[i] → rewire [[o,i]]
		ins → let (ins1, ins2) = splitAt (length ins `div` 2) ins in replace2 $ \o1 o2 → 
			[Node $ Duplicator {level = 0, inp = o, out1 = o1, out2 = o2},
			 Node $ Multiplier {out = o1, ins = ins1},
			 Node $ Multiplier {out = o2, ins = ins2}]

withoutIdx ∷ [a] → Int → [a]
withoutIdx xs i = let (ys,zs) = splitAt i xs in ys ⧺ tail zs

insertIdx ∷ Int → a → [a] → [a]
insertIdx i x xs = let (l,r) = splitAt i xs in l ⧺ [x] ⧺ r

split ∷ Int → Int → [a] → [[a]]
split i n [] = replicate n []
split i n xs = let (x,xs') = splitAt i xs in x : split i n xs'

transpose' n [] = replicate n []
transpose' n xs = transpose xs

commute ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
commute = do
	n1 :-: n2 ← activePair
	require (n1 ≢ n2)
	let aux1 = inspect n1 `withoutIdx` pp n1
	let aux2 = inspect n2 `withoutIdx` pp n2
	let es1 = length aux1
	let es2 = length aux2
	replace (es1 * es2) $ \edges → let
			edges1 = split es1 es2 edges
			edges2 = transpose' es1 edges1
		in [Node $ updateLevel n2 $ update (insertIdx (pp n1) pp1 auxs) n1 | (pp1,auxs) ← zip aux2 edges1]
		 ⧺ [Node $ updateLevel n1 $ update (insertIdx (pp n2) pp2 auxs) n2 | (pp2,auxs) ← zip aux1 edges2]
	where updateLevel you me = case me of
		Duplicator {} → maybeLevelUp
		Delimiter  {} → maybeLevelUp
		_ → me
		where maybeLevelUp = case you of
			Delimiter  {} → if level you ≤ level me then me {level = level me + 1} else me
			Abstractor {} → me {level = level me + 1}
			_ → me

annihilate ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
annihilate = do
	n1 :-: n2 ← activePair
	require (n1 ≡ n2)
	let aux1 = inspect n1 `withoutIdx` pp n1
	let aux2 = inspect n2 `withoutIdx` pp n2
	rewire $ [[a1,a2] | (a1,a2) ← aux1 `zip` aux2]

annihilateDelimiters ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
annihilateDelimiters = do
	rewrite ← annihilate
	Delimiter {} ← liftReader . inspectNode =<< previous
	return rewrite

eliminateDelimiter ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
eliminateDelimiter = do
	let
		eraser = do
			e@Eraser {} ← node
			return e
		constant = do
			c@Constant {} ← node
			return c
	n ← eraser <|> constant
	Delimiter {inp = iD} ← neighbour =<< previous
	require (inp n ≢ iD)
	replace0 [Node $ n {inp = iD}]

eliminateDuplicator ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
eliminateDuplicator = do
	Eraser {inp = iE} ← node
	Duplicator {inp = iD, out1 = o1, out2 = o2} ← neighbour =<< previous
	require (iE ≡ o1 ∨ iE ≡ o2)
	if iE ≡ o1
		then rewire [[iD,o2]]
		else rewire [[iD,o1]]

eraser ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
eraser = do
	rewrite ← commute
	Eraser {} ← liftReader . inspectNode =<< previous
	return rewrite

duplicate ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
duplicate = do
	rewrite ← commute
	Duplicator {} ← liftReader . inspectNode =<< previous
	return rewrite

beta ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
beta = do
	Abstractor {body = b, var = v} :-: Applicator {inp = ai, func = f, arg = a} ← activePair
	replace0
		[Node $ Delimiter {level = 0, inp = ai, out = b},
		 Node $ Delimiter {level = 0, inp = a, out = v}]

commuteDelimiterRed ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
commuteDelimiterRed = do
	rewrite ← commuteDelimiter
	ports ← liftReader . inspectNode . (!!1) =<< history -- next to previous
	require $ length (ports ∷ [Port]) ≤ 2
	return rewrite

commuteDelimiter ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
commuteDelimiter = do
	rewrite ← commute
	Delimiter {} ← liftReader . inspectNode =<< previous
	return rewrite

applyConstant ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
applyConstant = do
	Constant {name = n} :-: Applicator {inp = i, arg = a} ← activePair
	replace0 [Node $ Function {inp = i, out = a, name = n}]

applyFunction ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
applyFunction = do
	Function {inp = i, name = fn} :-: Constant {name = cn} ← activePair
	replace0 [Node $ Constant {inp = i, name = fn ⧺ " " ⧺ cn}]

-- | Not the readback semantics as defined in the paper. Just a non semantics preserving erasure of all
-- delimiters to make the graph more readable
readback ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
readback = do
	Delimiter {inp = i, out = o} ← node
	rewire [[i,o]]
