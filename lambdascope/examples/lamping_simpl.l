let
	h = λh. let
			w = λw. h (w (λy.y))
		in w (w (λz.z))
in h (h (λx.x))
