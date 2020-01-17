#include <iostream>


template <int N, int K>
class binomial_coefficient {
public:
    constexpr auto operator() () const -> int {
        return binomial_coefficient<N-1,K-1>()() * N / K;
    }
};


template <>
class binomial_coefficient<0, 0> {
public:
    constexpr auto operator() () const -> int {
        return 0;
    }

};


template <int N>
class binomial_coefficient<N, 0> {
public:
    constexpr auto operator() () const -> int {
        return 1;
    }
};


template <int N>
class binomial_coefficient<N, N> {
public:
    constexpr auto operator() () const -> int {
        return 1;
    }
};


auto main() -> int {
    constexpr int coeff = binomial_coefficient<19,7>()();
    std::cout << coeff << std::endl;
}