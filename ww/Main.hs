{-# LANGUAGE UnicodeSyntax, FlexibleInstances, FlexibleContexts #-}
module Main where

import GraphRewriting.Graph
import GraphRewriting.GL.Render
import GraphRewriting.GL.UI as UI
import Term (parseFile)
import Resolver (resolve)
import Graph
import GL ()
import Rules
import GraphRewriting.Rule hiding (erase)
import GraphRewriting.Pattern
import GraphRewriting.Graph.Read
import GraphRewriting.Graph.Write.Unsafe as Unsafe
import GraphRewriting.Layout.Coulomb
import GraphRewriting.Layout.SpringEmbedder
import GraphRewriting.Layout.Gravitation
import GraphRewriting.Layout.Wrapper


instance Render (Wrapper NodeWW) where render = render . wrappee

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
	Unsafe.adjustNode n $ Position . sf (\x → min 10 (x*0.9)) . cgf (\x → min 10 (x*0.01)) . cf (\x → min 10 (100/(x^2+0.1))) . position
	Unsafe.adjustNode n $ rot (*0.9)

unshare ∷ (View [Port] n, View NodeWW n) ⇒ Rule n
unshare = duplicatePrimitive <|> duplicateFunction where
	duplicateFunction = initDuplication >>> exhaustive (everywhere duplicate) >>> exhaustive (everywhere deactivate)
	duplicate = anyOf [duplicateAbstractor, duplicateApplicator, duplicateDuplicator, duplicateEraser, annihilate]

ruleTree = Branch "All"
	[Branch "Safe"
		[Leaf "Beta Reduction" beta,
	 	 Leaf "Apply Primitive" applyPrimitive,
	 	 Leaf "Unshare MFE" unshare,
	 	 Leaf "Eliminate" eliminate,
	 	 Leaf "Erase" erase],
	 Branch "Unshare MFE (unsafe)"
	 	[Leaf "Initiate" initDuplication,
	 	 Branch "Intermediate"
	    	 [Leaf "DuplicateAbstractor" duplicateAbstractor,
	     	  Leaf "DuplicateApplicator" duplicateApplicator,
	     	  Leaf "DuplicateDuplicator" duplicateDuplicator,
	     	  Leaf "DuplicateEraser" duplicateEraser,
	     	  Leaf "Annihilate" annihilate],
	 	 Leaf "Finalise" deactivate]]
