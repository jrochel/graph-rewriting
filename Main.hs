{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}
module Main where

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
import GraphRewriting.Layout.Wrapper


instance Render (Wrapper NodeLS) where render = render . wrappee

main ∷ IO ()
main = do
	(prog,args) ← UI.initialise
	file ← case args of
		[f] → return f
		___ → error "usage: vle [GLUT-options] <file>"
	term ← parseFile file
	let hypergraph = resolve term
	let graph = execGraph (apply $ exhaustive compileShare) (wrapGraph hypergraph)
	UI.run 50 id layoutStep graph ruleTree

layoutStep n = do
	(cgf, cf, sf, rot) ← readOnly $ do
		cgf ← centralGravitation n
		cf ← coulombForce n
		sf ← springForce 1.5 n
		rot ← angularMomentum n
		return (cgf, cf, sf, rot)
	Unsafe.adjustNode (Position . sf (\x → min 10 (x*0.9)) . cgf (\x → min 10 (x*0.01)) . cf (\x → min 10 (100/(x^2+0.1))) . position) n
	Unsafe.adjustNode (rot (*0.9)) n

ruleTree = Branch "All"
	[Leaf "Beta Reduction" beta,
	 Branch "All but Beta"
	 	[Leaf "Duplicate" duplicate,
	 	 Leaf "Eliminate" (eliminateDelimiter <|> eliminateDuplicator),
	 	 Leaf "Annihilate" annihilate,
	 	 Leaf "Commute Delimiter" commuteDelimiter,
	 	 Leaf "Erase" eraser, 
	 	 Branch "Primitive" [Leaf "Constant" applyConstant, Leaf "Function" applyFunction]],
	 Leaf "All but Beta (exhaustively)" $ exhaustive $ anyOf [duplicate, eliminateDelimiter, eliminateDuplicator, annihilate, commuteDelimiter, eraser, applyConstant, applyFunction]]
