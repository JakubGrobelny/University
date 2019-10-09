#include <iostream>


auto lucas_number(unsigned n) {
    if (n == 0)
        return 2;
    if (n == 1)
        return 1;
    return lucas_number(n-1) + lucas_number(n-2);
}

auto lucas_number_tail(unsigned n) {
 
    const auto aux = [](unsigned n, int l1, int l2, const auto& rec) {
        if (n == 0)
            return l1;
        if (n == 1)
            return l2;
        return rec(n - 1, l2, l1 + l2, rec);
    };

    return aux(n, 2, 1, aux);
}

auto main() -> int {
    for (int i = 0; i < 10; i++)
        std::cout << lucas_number(i) << std::endl;
    
    return 0;
}