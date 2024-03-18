let
	s = λd. let t = λe. d e
		in t (λco. λnst. co) (t (λk. λl. l))
in s (λc. s (λx. c)) 2 3
