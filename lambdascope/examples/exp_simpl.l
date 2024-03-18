let
	f x = let g y z = let h a = y (y a)
	                  in h (h z)
	      in g (g x)
in f (f (λx.x)) (λx.x)
