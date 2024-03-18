module Common.GL where

import GraphRewriting.Layout.PortSpec
import qualified Graphics.UI.GLUT as GL
import GraphRewriting.GL.Render


drawPorts ∷ PortSpec n ⇒ n → IO ()
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
