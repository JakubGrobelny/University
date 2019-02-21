function find(x)
    while x * (1.0 / x) == 1.0
        x = nextfloat(x)
    end
    print("$x : $(bitstring(x))\n")
end

find(1.0) 
# 1.000000057228997 : 0011111111110000000000000000000000001111010111001011111100101010