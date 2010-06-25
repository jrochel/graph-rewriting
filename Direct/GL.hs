module Direct.GL () where

import Common.GL
import Direct.Graph
import GraphRewriting.Layout.PortSpec
import qualified Graphics.UI.GLUT as GL
import GraphRewriting.GL.Render


instance PortSpec SKI where
	portSpec node = let sd = sameDir in case node of
		S {} → [sd n]
		K {} → [sd n]
		I {} → [sd n]
		Applicator {} → [sd n, sd (sw*0.7), sd (se*0.7)]
		Duplicator {} → [sd nw, sd ne, sd s]
		Eraser {} → [sd s]
		Variable {} → [sd n]
		Root {} → [sd s]
		where
			n = Vector2 0 1
			w = Vector2 (-1) 0
			e = Vector2 1 0
			s = Vector2 0 (-1)
			sw = Vector2 (-1) (-1)
			se = Vector2 1 (-1)
			nw = Vector2 (-1) 1
			ne = Vector2 1 1


instance Render SKI where render = renderNode

renderNode Root {} = return ()
renderNode node = drawPorts node >> case node of
	S {} → drawNode "S"
	K {} → drawNode "K"
	I {} → drawNode "I"
	Applicator {} → drawNode "A"
	Duplicator {} → GL.preservingMatrix $ GL.renderPrimitive GL.LineLoop $ do
		vertex2 (0,-1)
		vertex2 (-1,1)
		vertex2 (1,1)
	Eraser {} → drawNode "E"
	Variable {name = n} → drawNode n
