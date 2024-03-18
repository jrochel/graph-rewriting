{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
module Main where

import Prelude.Unicode
import Data.Foldable (toList)
import Data.Traversable (mapAccumL)
import Data.List (delete)
import GraphRewriting.Graph
import GraphRewriting.GL.Render
import GraphRewriting.GL.UI as UI
import Term (parseFile)
import Resolver (resolve)
import Graph
import GL ()
import Rules
import GraphRewriting.Rule
import GraphRewriting.Pattern
import GraphRewriting.Graph.Read
import GraphRewriting.Graph.Write.Unsafe as Unsafe
import GraphRewriting.Layout.Coulomb
import GraphRewriting.Layout.SpringEmbedder
import GraphRewriting.Layout.Gravitation
import GraphRewriting.Layout.Wrapper as Layout

import GraphRewriting.Strategies.Control as Control
import GraphRewriting.Strategies.LeftmostOutermost


instance Render n ⇒ Render (Layout.Wrapper n) where render = render . wrappee
instance PortSpec n ⇒ PortSpec (Control.Wrapper n) where portSpec = portSpec . wrapped
instance LeftmostOutermost n ⇒ LeftmostOutermost (Layout.Wrapper n) where lmoPort = lmoPort . wrappee


main ∷ IO ()
main = do
	(prog,args) ← UI.initialise
	let lmo = "--lmo" ∈ args
	args ← return $ "--lmo" `delete` args
	let bench = "--bench" ∈ args
	args ← return $ "--bench" `delete` args
	file ← case args of
		[f] → return f
		___ → error "usage: lambdascope [GLUT-options] [--lmo] [--bench] <file>"
	term ← parseFile file
	let hypergraph = execGraph (apply $ exhaustive compileShare) (resolve term)

	if bench
		then do
			let tree = lmoTree ruleTree
			let indexes = evalGraph (benchmark $ toList tree) (Control.wrapGraph hypergraph)
			print indexes
			let indexTable = foldl (flip incIndex) [] indexes
			print indexTable
			let (_, numTree) = mapAccumL (\(i:is) _ → (is,i)) (indexTable ⧺ repeat 0) tree
			putStrLn $ showLabelledTree 2 0 (+) numTree
		else let layoutGraph = Layout.wrapGraph hypergraph in if lmo 
			then UI.run 50 id layoutStep (Control.wrapGraph layoutGraph) (lmoTree ruleTree)
			else UI.run 50 id layoutStep layoutGraph ruleTree

incIndex ∷ Int → [Int] → [Int]
incIndex 0 (i:is) = i+1 : is
incIndex 0 [    ] = [1]
incIndex n (i:is) = i : incIndex (n-1) is
incIndex n [    ] = 0 : incIndex (n-1) []

-- | Modifies the rules of the rule tree with a given function.
-- This can be used to for example wrap a strategy rule around the existing rules.
mapRules ∷ (n → m) → LabelledTree n → LabelledTree m
mapRules f (Leaf n r)    = Leaf n (f r)
mapRules f (Branch n rs) = Branch n (map (mapRules f) rs)

-- Appends a rule to the top branch of a rule tree
appendRule ∷ n → LabelledTree n → LabelledTree n
appendRule r l@(Leaf n rr) = Branch n [l, Leaf "Move Control" r]
appendRule r (Branch n rs) = Branch n (rs ++ [Leaf "Move Control" r])

layoutStep ∷ (PortSpec n, View Position n, View Rotation n, View [Port] n) ⇒ Node → Rewrite n ()
layoutStep n = do
	(cgf, cf, sf, rot) ← readOnly $ do
		cgf ← centralGravitation n
		cf ← coulombForce n
		sf ← springForce 1.5 n
		rot ← angularMomentum n
		return (cgf, cf, sf, rot)
	Unsafe.adjustNode n $ Position . sf (\x → min 10 (x*0.9)) . cgf (\x → min 10 (x*0.01)) . cf (\x → min 10 (100/(x^2+0.1))) . position
	Unsafe.adjustNode n $ rot (*0.9)

lmoTree ∷ (LeftmostOutermost n, View [Port] n, View Control n) ⇒ LabelledTree (Rule n) → LabelledTree (Rule n)
lmoTree = appendRule moveControl . mapRules leftmostOutermost

ruleTree ∷ (View NodeLS n, View [Port] n) ⇒ LabelledTree (Rule n)
ruleTree = Branch "All"
	[Leaf "Beta Reduction" beta,
	 Branch "All but Beta"
	 	[Leaf "Duplicate" duplicate,
	 	 Leaf "Eliminate" (eliminateDelimiterEraser <|> eliminateDelimiterConstant <|> eliminateDuplicator),
	 	 Leaf "Annihilate" annihilate,
	 	 Leaf "Commute Delimiter" commuteDelimiter,
	 	 Leaf "Erase" eraser,
		 Leaf "Case" caseNode,
	 	 Branch "Primitive"
			[Leaf "Constant" applyConstant,
			 Leaf "Apply Operator" applyOperator,
			 Leaf "Exec Operator" execOperator,
			 Leaf "Reduce Operand" reduceOperand]]]
