setprecision(256) # 256-bitowa precyzja arytmetyki


# Biblioteka do rysowania wykresów
using Plots
plotly()


# Metoda obliczająca iloraz różnicowy z definicji (2) dla 
# zadanych punktów oraz wartości funkcji w tych punktach
function difference_quotient(points)
    xs = map(x -> BigFloat(x[1]), points)
    ys = map(y -> BigFloat(y[2]), points)
    n = size(xs)[1]

    res = BigFloat(0)
    
    for i in 1:n
        denominator = BigFloat(1)
        for j in 1:n
            if j != i
                denominator *= (xs[i] - xs[j])
            end
        end
        res += ys[i] / denominator
    end

    return res
end


# Funkcja obliczajca kolejne przybliżenie miejsca zerowego 
# funkcji f przy użyciu metody Müllera
function mullers_method(x0, x1, x2, f)
    if x1 == x2
        return :end
    end
   
    # Często używane wartości funkcji są zapamiętywane 
    # aby uniknąć ich wielokrotnego obliczania
    f0 = f(x0)
    f1 = f(x1)
    f2 = f(x2)

    numerator = BigFloat(2) * f2

    λ = difference_quotient([(x2, f2), (x1, f1)]) +
        difference_quotient([(x2, f2), (x0, f0)]) -
        difference_quotient([(x1, f1), (x0, f0)])
    x0_x1_x2_diff_quot = difference_quotient([
        (x0, f0), 
        (x1, f1), 
        (x2, f2)
    ])
    
    # Obliczanie obu możliwych wartości mianownika
    denom_r = sqrt(λ^2.0 - 4.0*f(x2) * x0_x1_x2_diff_quot + 0im)
    denom_0 = λ - denom_r
    denom_1 = λ + denom_r

    # Wybór większej wartości mianownika
    denominator = abs(denom_0) > abs(denom_1) ? denom_0 : denom_1

    return x2 - numerator/denominator
end


# Funkcja obliczająca kolejne przybliżenie miejsca zerowego
# funkcji f przy użyciu metody siecznych
function secant_method(x0, x1, f)
    if x1 == x0
        return :end
    end
    f0 = f(x0)
    f1 = f(x1)
    return x1 - f1 * (x1 - x0) / (f1 - f0)
end


# Pomocnicza funkcja do numerycznego wyznaczania 
# przybliżenia pochodnej funkcji f
function derivative(f)
    h = x + 0.00000001
    return x -> difference_quotient([(x, f(x)), (h, f(h))])
end


# Funkcja obliczająca kolejne przybliżenie miejsca zerowego funkcji f 
# przy użyciu metody Newtona używająca funkcji derivative(f) do 
# wyznaczenia pochodnej funkcji f
function newtons_method(x0, f)
    return x0 - f(x0) / derivative(f)(x0)
end


# Wersja funkcji obliczającej kolejne przybliżenie miejsca zerowego
# funkcji f, do której należy podać również pochodną funkcji f
function newtons_method(x0, f, df)
    return x0 - f(x0) / df(x0)
end


# Funkcja obliczająca liczbę poprawnych cyfr wyniku
function correct_digits(approx, accurate)
    delta = abs((accurate - approx) / accurate)
    return floor(-log10(delta))
end


# Funkcja obliczająca n kolejnych przybliżeń miejsca zerowego f
# dla wszystkich trzech metod
function calculate_results(f, df, x0, n)
    x1 = newtons_method(x0, f, df)
    x2 = secant_method(x0, x1, f)
    
    newton_results = [x0]
    secant_results = [x0, x1]
    muller_results = Union{BigFloat, Complex{BigFloat}}[x0, x1, x2]

    newton_finished = false
    secant_finished = false
    muller_finished = false
    
    for i in 1:n
        if newton_finished && secant_finished && muller_finished
            break
        end
        
        if !newton_finished
            x_i = newtons_method(newton_results[i], f, df)
            if x_i == newton_results[i]
                newton_finished = true
            else
                push!(newton_results, x_i)
            end
        end
                
        if !secant_finished
            x_i = secant_method(
                secant_results[i],
                secant_results[i+1],
                f
            )
            if x_i == :end
                secant_finished = true
            else
                push!(secant_results, x_i)
            end
        end

        if !muller_finished
            x_i = mullers_method(
                muller_results[i],
                muller_results[i+1],
                muller_results[i+2],
                f
            )
            if x_i == :end
                muller_finished = true
            else
                push!(muller_results, x_i)
            end
        end
    end

    return newton_results, secant_results, muller_results
end


# Pomocnicza funkcja wypełniająca tablice do danego rozmiaru aby
# ułatwić rysowanie wykresu
function fill_array(array, n)
    if size(array)[1] > n
        throw("fill_array: Invalid input!")
    else
        while size(array)[1] < n
            push!(array, BigFloat(Inf))
        end
    end
end


# Funkcja rysująca wykres ilości dokładnych cyfr wyniku dla 
# wszystkich trzech metod
function draw_plot(newton, secant, muller)
    n = max(size(newton)[1], size(secant)[1], size(muller)[1])
    fill_array(newton, n)
    fill_array(secant, n)
    fill_array(muller, n)
    plot(
        1:n,
        xticks=0:1:n,
        yticks=0:5:120,
        xlabel="\$n\$",
        ylabel="\$liczba\\, cyfr\$",
        label=["Newton" "sieczne" "Müller"],
        [newton, secant, muller],
        linewidth=2.0
    )
end


# Funkcja testująca wszystkie metody dla zadanej funkcji, jej pochodnej,
# przybliżenia początkowego, dokładnej wartości miejsca zerowego oraz 
# ilości iteracji
function test_methods(f, df, x0, zero, n)
    f = x -> BigFloat(f(x)) 
    df = x -> BigFloat(df(x))
    x0 = BigFloat(x0)

    newton, secant, muller = map(
        xs -> map(x -> correct_digits(x, zero), xs),
        calculate_results(f, df, x0, n)
    )
    
    function print_results(xs, name)
        println("==========================================")
        println("Wyniki dla metody $name:\n")
        i = 1
        for x in xs
            istr = lpad(i, log10(n), 0)
            println("n=$istr --- $(xs[i])"
            i += 1
        end
        println("==========================================")
    end
    
    print_results(newton, "Newtona")
    print_results(secant, "siecznych")
    print_results(muller, "Müllera")
end
 

# TESTY
f0(x) = x^2.0 - 3.0x + 2.0
df0(x) = 2.0x - 3.0
f0_zero = BigFloat(1.0)
x0_0 = BigFloat(-0.2)
@assert(f0(f0_zero) == 0.0)
test_methods(f0, df0, x0_0, f0_zero, 20)

f1(x) = sqrt(x)-4
df1(x) = 0.5*x^(-1.0/2.0)
f1_zero = 16.0
x0_1 = BigFloat(18.0)
@assert(f1(f1_zero) == 0.0, f1(f1_zero))
test_methods(f1, df1, x0_1, f1_zero, 20)

f2(x) = (x-1.0)*(x-2.0)*(2x-3.0)*(x-4.0)
df2(x) = 8.0(x^3.0 - 6.375x^2 + 12.25x - 7.25)
f2_zero = 2.0
x0_2 = BigFloat(3.0)
@assert(f2(f2_zero) == 0.0, f2(f2_zero))
test_methods(f2, df2, x0_2, f2_zero, 20)

f3(x) = x*(2.0^x)-2
df3(x) = 2.0^x * (x * log(2.0)+1.0)
f3_zero = BigFloat(1)
x0_3 = BigFloat(0.123)
@assert(f3(f3_zero) == 0.0, f3(f3_zero))
test_methods(f3, df3, x0_3, f3_zero, 20)

f4(x) = log(abs(x))
df4(x) = 1.0/x
f4_zero = 1.0
x0_4 = BigFloat(0.5)
@assert(f4(f4_zero) == 0.0, f4(f4_zero))
test_methods(f4, df4, x0_4, f4_zero, 20)

f5(x) = cos(x*sin(x))
df5(x) = sin(x*sin(x))*(-(sin(x)+x*cos(x)))
f5_zero = BigFloat(-π)/2
x0_5 = BigFloat(-1)
# @assert(f5(f5_zero) == 0.0, f5(f5_zero))
test_methods(f5, df5, x0_5, f5_zero, 20)
