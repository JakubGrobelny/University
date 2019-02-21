#include <iostream>
#include "tab_bit.hpp"

auto main() -> int
{
    tab_bit k(32);
    tab_bit t(32);
    
    try
    {
        std::cout << "Wprowadz k: ";
        std::cin >> k;
        std::cout << std::endl;

        std::cout << "Wprowadz t: ";
        std::cin >> t;
        std::cout << std::endl;
    }
    catch (std::invalid_argument exc)
    {
        std::cout << exc.what() << std::endl;
        return 1;
    }


    std::cout << k << " k " << std::endl;
    std::cout << t << " t " <<std::endl;
    std::cout << ~t << " = ~t" << std::endl;
    std::cout << std::endl;    

    std::cout << k << " k " << std::endl;
    std::cout << t << " t " <<std::endl;
    std::cout << (k&t) << " = k&t" << std::endl;
    std::cout << std::endl;    

    std::cout << k << " k " << std::endl;
    std::cout << t << " t " <<std::endl;
    std::cout << (k^t) << " = k^t" << std::endl;
    std::cout << std::endl;    

    std::cout << k << " k " << std::endl;
    std::cout << t << " t " <<std::endl;
    std::cout << (k|t) << " = k|t" << std::endl;
    std::cout << std::endl;    

    tab_bit q(uint64_t(11));
    std::cout << q << " q " << std::endl;
    std::cout << "q[4] = " << q[4] << std::endl << std::endl;
    std::cout << "q[3] <- 0" << std::endl;
    q[3] = false;
    std::cout << q << " q " << std::endl << std::endl;

    tab_bit r(q);
    std::cout << r << " r (kopia q)" << std::endl;

    std::cout << std::endl << "w z initializer_list: " << std::endl;
    tab_bit w({0,0,0,0,0,0,0,0,0,0,0,0,1,1});
    std::cout << w << " rozmiar = " << w.rozmiar() << std::endl << std::endl;

    tab_bit v(std::move(w));
    std::cout << v << " v (kopia w [przypisanie przenoszace])" << std::endl;

    v ^= v;
    std::cout << v << " v ^= v" << std::endl;

    k &= k;
    std::cout << k << " k &= k" << std::endl;

    k |= q;
    std::cout << k << " k |= q" << std::endl;

    q &= t;
    std::cout << q << " q &= t" << std::endl;

    try
    {
        std::cout << q[127] << std::endl;
    }
    catch (std::invalid_argument exc)
    {
        std::cout << "(Proba odczytania bitu spoza zakresu: " << std::endl;
        std::cout << exc.what() << std::endl;
    }

    tab_bit o = {0,1,0,1,0,1};
    std::cout << o << " o z initializer list" << std::endl;

    tab_bit l(o);
    std::cout << l << " l (kopia o)" << std::endl;

    std::cout << "l[0] = l[l.rozmiar() - 1]: " << std::endl;
    l[0] = l[l.rozmiar() - 1];
    std::cout << l << " l" << std::endl;

    l ^= o;
    std::cout << l << " l ^= o" << std::endl;

    l &= o;
    std::cout << l << " l &= o" << std::endl;

    return 0;
}