let
	flip f x y = f y x
	const x y = x
	id x = x
in flip const a id b
