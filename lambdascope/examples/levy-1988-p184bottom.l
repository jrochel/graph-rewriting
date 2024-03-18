let I0    = \x0.x0
    I1    = \x1.x1
    J     = \z.b
    Delta = \y.I1 (y y)
in (\f.(f I0)(f J)) (\x. Delta (x a))

