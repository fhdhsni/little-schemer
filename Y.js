const Y = le => (f => f(f))(f => le(x => f(f)(x)))
Y(fac => n => n === 1 ? 1 : n * fac(n - 1))
