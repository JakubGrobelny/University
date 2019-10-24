#include <iostream>
#include <array>
#include <random>
#include <iomanip>
#include <chrono>
#include <vector>


template <int N>
class matrix {
    double* arr;

    static auto rand_double() -> double {
        static std::mt19937 gen{std::random_device()()};
        return std::uniform_real_distribution<double>(0.5, 2.0)(gen);
    }

public:

    auto operator=(const matrix&) -> matrix& = delete;
    matrix(const matrix&);

    auto operator=(matrix&& m) -> matrix& {
        if (this != &m) {
            delete[] this->arr;
            this->arr = m.arr;
            m.arr = nullptr;
        }
        return m;
    }

    matrix(matrix&& m) {
        this->arr = m->arr;
        m->arr = nullptr;
    }

    matrix() : arr(new double[N*N]) {
        for (size_t row = 0; row < N; row++) {
            for (size_t col = 0; col < N; col++) {
                this->arr[row * N + col] = rand_double();
            }
        }
    }

    ~matrix() {
        delete[] arr;
    }

    auto square() -> matrix& {
        auto res = new double[N*N];

        for (size_t i = 0; i < N; i++) {
            for (size_t j = 0; j < N; j++) {
                res[i*N + j] = 0.0;
                for (size_t k = 0; k < N; k++) {
                    res[i*N + j] += arr[i*N + k] * arr[k*N + j];
                }
            }
        }

        delete[] this->arr;
        this->arr = res;
        return *this;
    }

    template <int M>
    friend auto operator<< (std::ostream&, const matrix<M>&) -> std::ostream&;

    auto operator[] (size_t row) const -> double* {
        return this->arr + row * N;
    }
};


template <int N>
auto operator<< (std::ostream& out, const matrix<N>& m) -> std::ostream& {
    out <<  std::setfill(' ');
    for (size_t i = 0; i < N; i++) {
        for (size_t j = 0; j < N; j++) {
            out << std::setw(10) << m[i][j] << ' ';
        }
        out << std::endl;
    }
    return out;
}


template <int N>
void measure(matrix<N>& m, size_t iterations) {
    using namespace std::chrono;
    
    auto t0 = high_resolution_clock::now();

    for (size_t i = 0; i < iterations; i++) {
        m.square();
    }

    auto t1 = high_resolution_clock::now();

    duration<double> time_span = duration_cast<duration<double>>(
        (t1 - t0) / iterations
    );

    std::cout << N 
              << " multiplication took " 
              << time_span.count() 
              << " seconds." 
              << std::endl;
}


auto main() -> int {
    matrix<10> m10;
    measure(m10, 10000);
    
    matrix<100> m100;
    measure(m100, 100);
    
    matrix<1000> m1000;
    measure(m1000, 1);

    // matrix<10000> m10000;
    // measure(m10000, 1);
}