function solve(A, b)
    A = [A b]
    rows = size(A,1)
    factor = A[1,1] / A[1,1]

    for j = 1:rows
        for i = j+1:rows
            factor = -A[i,j] / A[j,j]
            A[i,:] = A[i,:] + A[j,:] * factor
        end
    end

    U = A[:,1:end .!= rows+1]
    x = A[:,rows+1]
    res = zeros(rows)

    for i = rows:-1:1
        res[i] = x[i]
        for j = (i+1):rows
            res[i] -= U[i,j] * res[j]
        end
        res[i] /= U[i,i]
    end

    return res
end

A = [1 3 1
    1 1 -1
    3 11 6]

b = [9, 1, 35]

A1 = rand(1000,1000)
b1 = rand(1000)

println(A\b)
println(solve(A, b))

@time A1\b1
@time solve(A1, b1)
