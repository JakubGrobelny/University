setprecision(256)

# Przybliżenie liczby e używając granicy (1+1/n)^n
function calc_e_lim(n)
    (1 + 1/n)^n
end

# Przybliżenie liczby e używając szeregu 1 + 1/1! + 1/2! + ...
function calc_e_series(n)
    fact = 1.0
    sum = fact
    for k in 1:n
        fact *= k
        sum += 1/fact
    end
    return sum
end

# Funkcja używana do testowania zadanej metody f wykonując n iteracji
function test(f, n)
    e = f(BigFloat(n))
    Δe = BigFloat(ℯ) - e 
    relative_error = abs(Δe/e)
    accuracy = floor(-log10(relative_error))
    println("n=$n:\n$e\nSignif. digits: $accuracy\n")
end

for k in 1:10
    test(calc_e_lim, 2^k)
end

println("======================================================")

for k in 1:10:100
    test(calc_e_series, k)
end

println("======================================================")

# Wykresy

using Plots
plotly()

# Wykres wyników calc_e_lim dla n = 2^0, 2^1, ..., 2^10
xs = 0:10
range = map(x -> BigFloat(2.0)^x, xs)
approx = map(x -> calc_e_lim(x), range)
p = plot(
    xs, 
    xticks=0:1:10, 
    yticks=2.0:0.05:3.0, 
    ylims=(2.0, 2.8),
    map(x -> BigFloat(MathConstants.e), xs),
    xlabel="\$log_2\\,n\$", 
    ylabel="\$e\\_calc\\_lim(n)\$", 
    seriestype="line",
    label = "Dokładna wartość e"
)
scatter!(
    xs,
    approx,
    label = "Przybliżona wartość e"
)

# Wykres wyników calc_e_series dla n = 0, 1, ..., 20
xs = 0:10
approx = map(x -> calc_e_series(BigFloat(x)), xs)
plot(
    xs, 
    xticks=0:1:10, 
    yticks=2.0:0.05:3.0, 
    xlims=(0, 10),
    ylims=(2.0, 2.8),
    map(x -> BigFloat(MathConstants.e), xs),
    xlabel="\$n\$", 
    ylabel="\$e\\_calc\\_series(n)\$", 
    seriestype="line",
    label = "Dokładna wartość e"
)
scatter!(
    xs,
    approx,
    label = "Przybliżona wartość e"
)

# Wykres porównujący wyniki calc_e_lim i calc_e_series
plot(
    0:20, 
    xticks=0:2:20, 
    yticks=1.0:0.1:3.0, 
    xlims=(0, 20),
    ylims=(1.0, 2.8),
    [
        map(x -> BigFloat(MathConstants.e), 0:20),
        map(x -> calc_e_lim(BigFloat(x)), 0:20),
        map(x -> calc_e_series(BigFloat(x)), 0:20)
    ],
    xlabel="\$n\$", 
    ylabel="\$\\widetilde{e}\$", 
    label = ["Dokładna wartość e" "calc_e_lim(n)" "calc_e_series(n)"],
    lw = 1
)

# Wykres porównujący wyniki calc_e_lim i calc_e_series, przy założeniu, że
# funkcja calc_e_lim w rzeczywistości ma złożoność O(logn).
plot(
    0:15, 
    xticks=0:2:15, 
    yticks=1.0:0.1:3.0, 
    xlims=(0, 15),
    ylims=(1.0, 2.8),
    [
        map(x -> BigFloat(MathConstants.e), 0:15),
        map(x -> calc_e_lim(2^BigFloat(x)), 0:15),
        map(x -> calc_e_series(BigFloat(x)), 0:15)
    ],
    xlabel="\$liczba\\,\\,iteracji\$", 
    ylabel="\$\\widetilde{e}\$", 
    label = ["Dokładna wartość e" "calc_e_lim(2^n)" "calc_e_series(n)"],
    lw = 1
)

