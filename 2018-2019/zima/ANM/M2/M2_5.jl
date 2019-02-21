setprecision(256)

function P(k)

    if k == 2
        return BigFloat(2.0)
    end

    return BigFloat(2.0)^(k-1) * s(k)

end

function s(k)
    
    if k == 2
        return BigFloat(1.0)
    end

    return sqrt(BigFloat(0.5) * (BigFloat(1.0) - c(k-1)))

end

function c(k)
    
    if k == 2
        return BigFloat(0.0)
    end

    return sqrt(BigFloat(0.5) * (BigFloat(1.0) + c(k-1)))

end

function test()

    best = 2
    best_res = P(best)
    best_diff = abs(BigFloat(π) - best_res)

    for k in 7:11
        prec = 2^k
        setprecision(prec)
        for i in 3:2*precision(BigFloat)
            res = P(i)
            diff = abs(BigFloat(π) - res)
            if diff < best_diff
                best = i
                best_diff = diff
                best_res = res
            end
        end
        print("Float$prec:\nbest:$best \ndelta:$best_diff\n$best_res\n\n")
    end
end

# 