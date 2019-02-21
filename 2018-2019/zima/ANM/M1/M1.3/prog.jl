function w1(x)
    x^3 - 6*x^2 + 3*x - typeof(x)(0.149)
end

function w2(x)
    ((x-6)*x + 3) * x - typeof(x)(0.149)
end

x = 4.71
correct = -14.636489

function test(f, t)
    result = f(t(x))

    function absoluteerror(x1, x2)
        abs(x1 - x2)
    end

    print("$t:\t\nw($x) = $result\t\nΔw($x) = $(absoluteerror(correct, result))\n\n")
end

functions = [w1, w2]
types = [Float16, Float32, Float64]

print("accurate w($x) = $correct\n\n")

for f in functions
    for t in types
        test(f, t)
    end
end

# Float16:
# w1(4.71) = -14.58
# Δw1(4.71) = 0.058363999999999194

# Float32:
# w1(4.71) = -14.6365
# Δw1(4.71) = 1.1358581543774449e-5

# Float64:
# w1(4.71) = -14.636489000000006
# Δw1(4.71) = 7.105427357601002e-15

# Float16:
# w2(4.71) = -14.63
# Δw2(4.71) = 0.0036764999999991943

# Float32:
# w2(4.71) = -14.63649
# Δw2(4.71) = 8.681640633056986e-7

# Float64:
# w2(4.71) = -14.636489
# Δw2(4.71) = 0.0