using QuadGK
using Polynomials


function RPoly(k,n)
    p = Poly(1)
    for j in 0:n 
        if j != k
            p *= (Poly([-j, 1]))
        end
    end
    return p
end


function Z5(f, n, a, b)
    h = (b - a)/n
    A(k) = (h*(-1)^(n-k))/(factorial(k)*factorial(n - k)) * quadgk(x -> (RPoly(k,n))(x), 0, n)[1]
    Q = 0
    for k in 0:n
        Q += A(k)*f(a + k*h)
    end
    return Q
end


function f(x)
    return 1/(1+x*x)
end


# setprecision(256)

println(Z5(f, 1, -4, 4))
println(Z5(f, 3, -4, 4)) # najlepiej
println(Z5(f, 5, -4, 4))
println(Z5(f, 7, -4, 4))
println(Z5(f, 9, -4, 4))

# 0.47058823529411764
# 2.2776470588235296
# 2.3722292496158546
# 2.799700782497697
# 2.4308411566464567

