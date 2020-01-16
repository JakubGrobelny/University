#include <iostream>


template <int N>
class lucas_number {
    using prev = lucas_number<N-1>;
    static const int prev_val = prev::val;
    static const int val = prev::val + prev::prev_val;
public:
    constexpr auto operator() () const -> int {
        return lucas_number<N>::val;
    }
    friend class lucas_number<N+1>;
};


template <>
class lucas_number<0> {
    static const int val = 2;
public:
    constexpr auto operator() () const -> int {
        return lucas_number<0>::val;
    }
    friend class lucas_number<1>;
};


template <>
class lucas_number<1> {
    static const int val = 1;
    static const int prev_val = 2;
public:
    constexpr auto operator() () const -> int {
        return lucas_number<1>::val;
    }
    friend class lucas_number<2>;
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