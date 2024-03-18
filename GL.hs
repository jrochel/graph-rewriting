{-# LANGUAGE UnicodeSyntax #-}
module GL () where

import Graph
import GraphRewriting.Layout.PortSpec
import qualified Graphics.UI.GLUT as GL
import GraphRewriting.GL.Render


instance PortSpec NodeWW where
	portSpec node = let sd = sameDir in case node of
		Initiator  {} → [sd s]
		Applicator {} → [sd n, sd s, sd e]
		Abstractor {} → [sd n, sd s, sd e]
		Primitive  {} → [sd n]
		Eraser     {} → [sd n]
		Duplicator {} → [sd n, (sw, s), (se, s)]
		where
			n = Vector2 0 1
			e = Vector2 1 0
			s = Vector2 0 (-1)
			sw = Vector2 (-1) (-1)
			se = Vector2 1 (-1)

instance Render NodeWW where render = renderNode

renderNode node = drawPorts node >> case node of
	Initiator  {} → drawNode "I"
	Applicator {} → drawNode "@"
	Abstractor {} → drawNode (name node)
	Primitive  {} → drawNode (name node)
	Eraser     {} → drawNode ""
	Duplicator {} → do
		GL.preservingMatrix $ GL.renderPrimitive GL.LineLoop $ do
			vertex2 (0,1)
			vertex2 (-1,-1)
			vertex2 (1,-1)
		renderString $ if active node then "*" else ""

drawPorts ∷ NodeWW -> IO ()
drawPorts = mapM_ drawPort . relPortPos

circle r1 r2 step = mapM_ vertex2 vs where
	is = take (truncate step + 1) [0, i' .. ]
	i' = 2 * pi / step
	vs = [ (r1 * cos i, r2 * sin i) | i <- is ]

drawPort pos = GL.preservingMatrix $ do
	GL.translate $ vector pos
	GL.renderPrimitive GL.Polygon (circle 0.15 0.15 10)

drawNode label = do
	GL.renderPrimitive GL.LineLoop (circle 1 1 20)
	renderString label
