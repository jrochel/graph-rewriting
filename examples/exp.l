let
	id    = λx.x
	two   = λg y. g (g y)
	three = λf x. f (f (f x))
in three two two id id
