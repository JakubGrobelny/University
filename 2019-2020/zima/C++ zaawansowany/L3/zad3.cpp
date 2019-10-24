#include <iostream>
#include <ratio>


template <int N>
struct harmonic : public std::ratio_add<std::ratio<1, N>, harmonic<N-1>> {};

template <>
struct harmonic<1> : public std::ratio<1, 1> {};


auto main() -> int {
    /*
      H1 =     1,
      H2 =    3/2,
      H3 =   11/6,
      H4 =   25/12,
      H5 =  137/60,
      H6 =   49/20,
      H7 =  363/140,
      H8 =  761/280,
      H9 = 7129/2520,
    */
    constexpr auto n = 46;
    using h_n = harmonic<n>;
    std::cout << "H_" << n << " = " << h_n::num << "/" << h_n::den << std::endl;
}