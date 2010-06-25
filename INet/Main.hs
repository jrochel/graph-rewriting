{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}
-- Given the graph signature in INet/Graph.hs, the rewrite rules in INet/Rules.hs and the layouting information and rendering code in INet/GL.hs we can tie it together to obtain an interactive, graphical application to see our rewriting system in action.
module Main where

import GraphRewriting.GL.Render
import GraphRewriting.GL.UI as UI
import Common.Term (parseFile)
import INet.Graph
import INet.GL ()
import INet.Rules
import GraphRewriting
import GraphRewriting.Graph.Write.Unsafe as Unsafe
import GraphRewriting.Layout.Coulomb
import GraphRewriting.Layout.SpringEmbedder
import GraphRewriting.Layout.Gravitation
import GraphRewriting.Layout.Wrapper


-- This is annoying. You have to define how your wrapped nodes can be rendered namely by using the rendering code specified for the wrapped values. Maybe I will get rid of this requirement somehow. What is the wrapping about? In order to layout the nodes of your graph there has to be layouting information (position, orientation) associated to it, which we did not do in INet/Graph.hs. Therefore we use the 'wrapGraph' function (see below) to augment our nodes with this information.
instance Render (Wrapper SKI) where render = render . wrappee

main ∷ IO ()
main = do
	 -- first we need to initialise the GUI (GLUT really), which returns the program name and the arguments supplied to the program call (minus any GLUT options).
	(prog,args) ← UI.initialise
	file ← case args of
		[f] → return f
		___ → error "usage: ski [GLUT-options] <file>"
	term ← parseFile file
	let graph = fromTerm term -- here we compile the parsed term into a graph (see INet/Graph.hs)
	-- and finally we run the GUI.
	UI.run
		40                -- Here we specify that layoutStep should be applied 40 times to the initial random layout of the graph before displaying it.
		id                -- We use the identity projection. You don't have to care about this.
		layoutStep        -- This specifies the modification of the graph's layout, which is applied in every frame of the animation.
		(wrapGraph graph) -- Our graph, wrapped in paper.
		ruleTree          -- The rule menu you will see in the top left corner of the window.

-- Here we specify the forces that are applied in each layout step. You can play around with the values, but strange things may happen (correction: strange things already DO happen).
layoutStep n = do
	(cgf, cf, sf, rot) ← readOnly $ do
		cgf ← centralGravitation n -- this is needed in order to prevent unconnected subgraphs to drift apart infinitely
		cf ← coulombForce n        -- a repulsing force between each pair of nodes
		sf ← springForce 1.5 n     -- this tries to push nodes into the position as dictated by the edges connected to other nodes.
		rot ← angularMomentum n    -- this tries to rotate nodes such that the angular forces exercised by the edges upon the nodes are minimised.
		return (cgf, cf, sf, rot)
	-- combine all the forces specifying for each of them a function that maps the distance the force bridges to a strength.
	Unsafe.adjustNode n $ Position . sf (*0.9) . cgf (*0.01) . cf (\x → min (100/(x^2+0.1)) 10) . position
	-- We do not rotate our nodes here, since an SKI combinator graph can nicely be drawn top-down. Uncomment, if you like.
	-- Unsafe.adjustNode (rot (*0.9)) n

-- anyRule = anyOf [ eliminate, ruleI, ruleK0, ruleK1, ruleS0, ruleS1, ruleS2, ruleE0, ruleE1, ruleE2, ruleD0, ruleD1, ruleD2]

-- The menu that appears at the top-left corner of the window allows it to select specific rules that we defined in INet/Rules.hs. It comes in form of a tree, where the parent of a subtree is the disjunction of the subtree's rules.
ruleTree = Branch "All"
	[Leaf "Eliminate" eliminate,
	 Branch "Erase" [Leaf "E0" ruleE0, Leaf "E1" ruleE1, Leaf "E2" ruleE2],
	 Branch "S" [Leaf "S0" ruleS0, Leaf "S1" ruleS1, Leaf "S2" ruleS2],
	 Branch "K" [Leaf "K0" ruleK0, Leaf "K1" ruleK1],
	 Leaf "I" ruleI,
	 Branch "D" [Leaf "D0" ruleD0, Leaf "D1" ruleD1, Leaf "D2" ruleD2]]

-- That's it. I hope the tutorial was helpful. If you need any help in defining your own rewriting system, feel free to ask me (jan@rochel.info). Corrections / extensions of the library or the example applications are always welcome. Have fun with it!
--
-- Jan
