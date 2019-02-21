T(f, a, b) = (b-a)/2 * (f(a) + f(b))
Q(f, a, b) = (b - a)/6 * (f(a) + 4f((a + b)/2) + f(b))

function Simpson(m)
    h = pi/(2m)
    sum = 0

    for i = 0:m-1
        a = 2*i*h
        b = 2(i+1)*h
        sum += Q(sin, a, b)
    end

    return sum
end

function Trapezoidal(n)
    h = pi/n
    sum = 0.0

    for i = 0:n-1
        a = i*h
        b = (i+1)h
        sum += T(sin, a, b)
    end

    return sum
end