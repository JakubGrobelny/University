function method1(x0, x1, f) 
    fn = f(x1)
    x1 - fn * ((x1 - x0) / (fn - f(x0)))
end

function method2(x0, x1, f)
    fn = f(x1)
    fn0 = f(x0)
    (fn * x0 - fn0 * x1) / (fn - fn0)
end

function iter(a, b, iter_function, f, n)
    x0 = a
    x1 = b
    for i in 1:n
        val = iter_function(x0, x1, f)
        x0 = x1
        x1 = val
        if isnan(x1)
            return i, x0
        end
    end
    return x1
end

f(x) = x^2 - 612
g(x) = six(x) / x
h(x) = x * sin(x)