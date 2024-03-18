let
	Cons x xs = λcons nil. cons x xs
	Nil = λcons nil. nil
in (Cons 1 (Cons 2 (Cons 3 Nil))) (λx xs. x) 0
