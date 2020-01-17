#include <iostream>


template <int N>
struct lucas_number {
    static const int value = lucas_number<N-1>::value + 
                             lucas_number<N-2>::value;
    constexpr auto operator() () const -> int {
        return lucas_number<N>::value;
    }
};


template <>
struct lucas_number<0> {
    static const int value = 2;
    constexpr auto operator() () const -> int {
        return lucas_number<0>::value;
    }
};


template <>
struct lucas_number<1> {
    static const int value = 1;
    constexpr auto operator() () const -> int {
        return lucas_number<1>::value;
    }
};


// Alternative solution without templated functors
constexpr auto lucas(int n) -> int {
    int l0 = 2;
    int l1 = 1;
    for (int i = 0; i < n; i++) {
        int temp = l0;
        l0 = l1;
        l1 = temp + l1;
    }
    return l0;
}


auto main() -> int {
    constexpr int lucas0 = lucas_number<42>()();
    constexpr int lucas1 = lucas(42);
    std::cout << lucas0 << ' ' << lucas1 << std::endl;
}