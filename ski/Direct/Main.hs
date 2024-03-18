{-# LANGUAGE UnicodeSyntax, FlexibleInstances, FlexibleContexts #-}
module Main where

import GraphRewriting.GL.Render
import GraphRewriting.GL.UI as UI
import Common.Term (parseFile)
import Direct.Graph
import Direct.GL ()
import Direct.Rules
import GraphRewriting
import GraphRewriting.Graph.Write.Unsafe as Unsafe
import GraphRewriting.Layout.Coulomb
import GraphRewriting.Layout.SpringEmbedder
import GraphRewriting.Layout.Gravitation
import GraphRewriting.Layout.Wrapper


instance Render (Wrapper SKI) where render = render . wrappee

main ∷ IO ()
main = do
	(prog,args) ← UI.initialise
	file ← case args of
		[f] → return f
		___ → error "usage: ski [GLUT-options] <file>"
	term ← parseFile file
	let graph = fromTerm term
	UI.run 40 id layoutStep (wrapGraph graph) ruleTree          

layoutStep n = do
	(cgf, cf, sf, rot) ← readOnly $ do
		cgf ← centralGravitation n
		cf ← coulombForce n
		sf ← springForce 1.5 n
		rot ← angularMomentum n
		return (cgf, cf, sf, rot)
	Unsafe.adjustNode n $ Position . sf (*0.9) . cgf (*0.01) . cf (\x → min (100/(x^2+0.1)) 10) . position
	-- Unsafe.adjustNode (rot (*0.9)) n

ruleTree = Branch "All"
	[Leaf "S" ruleS, Leaf "K" ruleK, Leaf "I" ruleI,
	 Branch "Duplicate" [Leaf "Combinator" duplicateCombinator, Leaf "Applicator" duplicateApp],
	 Branch "Erase" [Leaf "Combinator" eraseCombinator, Leaf "Applicator" eraseApp],
	 Leaf "Eliminate" eliminate]
