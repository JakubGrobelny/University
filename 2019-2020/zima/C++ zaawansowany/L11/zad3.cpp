#include <iostream>


template <int A, int B>
class gcd {
public:
    constexpr auto operator() () const -> int {
        return gcd<B, A % B>()();
    };
};


template <int A>
class gcd<A, 0> {
public:
    constexpr auto operator() () const -> int {
        return A;
    };
};


auto main() -> int {
    constexpr int gcd_val = gcd<1778, 232>()();
    std::cout << gcd_val << std::endl;
}