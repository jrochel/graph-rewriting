{-# LANGUAGE UnicodeSyntax #-}
module GL () where

import Graph
import GraphRewriting.Layout.PortSpec
import qualified Graphics.UI.GLUT as GL
import GraphRewriting.GL.Render


instance PortSpec NodeLS where
	portSpec node = let sd = sameDir in case node of
		Initiator  {} → [sd s]
		Applicator {} → [sd n, sd s, sd e]
		Abstractor {} → [sd n, sd s, sd e]
		Constant   {} → [sd n]
		Function   {} → [sd n, sd s]
		Eraser     {} → [sd n]
		Duplicator {} → [sd n, (sw, s), (se, s)]
		Delimiter  {} → [sd $ Vector2 0 0.6, sd $ Vector2 0 (-0.6)]
		where
			n = Vector2 0 1
			w = Vector2 (-1) 0
			e = Vector2 1 0
			s = Vector2 0 (-1)
			sw = Vector2 (-1) (-1)
			se = Vector2 1 (-1)

instance Render NodeLS where render = renderNode

renderNode node = drawPorts node >> case node of
	Initiator  {} → drawNode "I"
	Applicator {} → drawNode "A"
	Abstractor {} → drawNode "L"
	Constant   {} → drawNode (name node)
	Function   {} → drawNode (name node)
	Eraser     {} → drawNode ""
	Duplicator {} → do
		GL.preservingMatrix $ GL.renderPrimitive GL.LineLoop $ do
			vertex2 (0,1)
			vertex2 (-1,-1)
			vertex2 (1,-1)
		drawString $ show $ level node
	Delimiter {} → do
		GL.renderPrimitive GL.LineStrip $ do
			vertex2 (-0.8,-0.3)
			vertex2 (-0.8,0)
			vertex2 (0.8,0)
			vertex2 (0.8,-0.3)
		drawString $ show $ level node

drawPorts ∷ NodeLS -> IO ()
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
	drawString label

drawString label = GL.preservingMatrix $ do
	GL.translate $ vector2 (-0.3,-0.3)
	GL.scale 0.007 0.007 (0 ∷ GL.GLdouble)
	GL.renderString GL.MonoRoman label
