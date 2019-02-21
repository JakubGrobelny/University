function bitstringtofloat(s)
    strexp = s[2:12]
    strfrac = s[13:end]

    sgn = 
    if s[1] - '0' == 0
        1.0
    else
        -1.0
    end
        
    exp = 0

    for i in [1:11;]
        chr = strexp[i]
        digit = strexp[i] - '0'
        exp |= digit << (11 - i)
    end

    exp -= 1023

    mantiss = 1.0

    for i in [1:52;]
        digit = strfrac[i] - '0'
        mantiss += digit * 2.0^(-i)
    end

    print("sign: $sgn, mantiss: $mantiss, exp: $exp\n")

    sgn * mantiss * 2.0^exp
end

# Tests:

str = bitstring(7.255)
print("$str\n")
f = bitstringtofloat(str)
print("$f\n")


