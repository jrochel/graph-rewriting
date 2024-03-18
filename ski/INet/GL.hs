-- Here we specify the OpenGL code to draw the nodes, and where the ports of the nodes are positioned, which is needed for the layouting.
module INet.GL () where

import Common.GL
import INet.Graph
import GraphRewriting.Layout.PortSpec
import qualified Graphics.UI.GLUT as GL
import GraphRewriting.GL.Render


-- Here we specify for each node type the position of its ports and the direction an edge attached to the port is to be oriented. Here we only use the 'sameDir' function that orients the edge in the same direction as the port, i.e. if the (only) port of the root node type (R) is south, also the edge attached to the port is to point south.
instance PortSpec SKI where
	portSpec node = let sd = sameDir in case node of
		R {} → [sd s]
		A {} → [sd n, sd (sw*0.7), sd (se*0.7)]
		I {} → [sd n]
		E {} → [sd s]
		D {} → [sd nw, sd ne, sd s]
		V {} → [sd n]
		K0 {} → [sd n]
		K1 {} → [sd n, sd s]
		S0 {} → [sd n]
		S1 {} → [sd n, sd s]
		S2 {} → [sd n, sd (sw*0.7), sd (se*0.7)]
		where -- points of compass
			n = Vector2 0 1
			s = Vector2 0 (-1)
			sw = Vector2 (-1) (-1)
			se = Vector2 1 (-1)
			nw = Vector2 (-1) 1
			ne = Vector2 1 1

-- Here we specify the OpenGL code that draws a node. Refer to the documentation of the OpenGL and GLUT packages on how to do this.
instance Render SKI where render = renderNode

renderNode R {} = return ()
renderNode node = drawPorts node >> case node of
	A {}  → drawNode "A"
	I {}  → drawNode "I"
	E {}  → drawNode "E"
	D {}  → GL.preservingMatrix $ GL.renderPrimitive GL.LineLoop $ do
		vertex2 (0,-1)
		vertex2 (-1,1)
		vertex2 (1,1)
	V {name = n}  → drawNode n
	K0 {} → drawNode "K"
	K1 {} → drawNode "K"
	S0 {} → drawNode "S"
	S1 {} → drawNode "S"
	S2 {} → drawNode "S"

-- Now you can see in INet/Main.hs how it all is tied together to make an actual graphical application out of this.
