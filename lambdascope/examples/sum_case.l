let
  sum list = case list of Nil -> 0
                          Cons z zs -> (\x.λy -> plus x y) z (sum zs)
in sum (Cons 1 (Cons 2 Nil))
