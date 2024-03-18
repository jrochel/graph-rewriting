let
	Cons x xs = λcons nil. cons x xs
	Nil = λcons nil. nil
	sum list = list (λx xs . (λx y . Plus x y) x (sum xs)) 0
in sum (Cons 1 Nil)
