{-# LANGUAGE UnicodeSyntax #-}
module GL () where

import Graph
import GraphRewriting.Layout.PortSpec
import qualified Graphics.UI.GLUT as GL
import GraphRewriting.GL.Render


instance PortSpec Vertex where
	portSpec node = let sd = sameDir in case node of
		Applicator {} → [sd n, sd (sw*0.7), sd (se*0.7)]
		Variable   {} → [sd n]
		Root       {} → [sd s]
		where
			n = Vector2 0 1
			w = Vector2 (-1) 0
			e = Vector2 1 0
			s = Vector2 0 (-1)
			sw = Vector2 (-1) (-1)
			se = Vector2 1 (-1)
			nw = Vector2 (-1) 1
			ne = Vector2 1 1

instance Render Vertex where render = renderNode

renderNode Root {} = return ()
renderNode node = drawPorts node >> case node of
	Applicator {} → drawNode "*"
	Variable   {} → drawNode [name node]

drawPorts ∷ Vertex -> IO ()
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
