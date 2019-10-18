#include <iostream>
#include <memory>
#include <string>
#include <vector>
#include <random>


class counter {
    uint64_t i = 1;

public:

    friend auto operator<< (std::ostream&, const counter&) -> std::ostream&;

    auto operator*= (uint64_t n) -> counter& {
        this->i *= n;
        return *this;
    } 
};


auto operator<< (std::ostream& out, const counter& c) -> std::ostream& {
    out << c.i;
    return out;
}


auto random(int min, int max) -> int {
    static std::mt19937 gen{std::random_device()()};
    return std::uniform_int_distribution<int>{min, max}(gen);
}


void multiply_by_primes(
    std::unique_ptr<counter[]>& arr, 
    size_t len, 
    unsigned m, 
    unsigned i
) {
    if (m == i) 
        return;

    static std::vector<uint64_t> primes = {2};

    const auto add_primes = [&](size_t how_many) -> void {
        int prime_candidate = primes.back();
        bool is_prime = true;

        while (primes.size() < how_many) {
            for (auto prime : primes) {
                if (prime_candidate % prime == 0) {
                    is_prime = false;
                    break;
                }
            }

            if (is_prime) {
                primes.push_back(prime_candidate);
            } else {
                is_prime = true;
            }

            prime_candidate++;
        }
    };

    if (primes.size() <= i) {
        add_primes(i+1);
    }

    for (size_t j = 0; j < len; j++) {
        if (random(0, 1)) {
            arr[j] *= primes[i];
        }
    }

    multiply_by_primes(arr, len, m, i+1);
}


auto main(int argc, char* argv[]) -> int {

    if (argc < 3) {
        std::cerr << "Not enough arguments!" << std::endl;
        return EXIT_FAILURE;
    }

    int n, m;

    try {
        n = std::stoi(argv[1]);
        m = std::stoi(argv[2]);
    } catch (std::exception& e) {
        std::cerr << "Failed to parse the numbers: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    std::unique_ptr<counter[]> arr{new counter[n]};

    if (!arr) {
        std::cerr << "Failed to allocate the array!";
        return EXIT_FAILURE;
    }

    multiply_by_primes(arr, n, m, 0);

    for (int i = 0; i < n; i++) {
        std::cout << arr[i] << ", ";
    }
    std::cout << std::endl;

    return EXIT_SUCCESS;
}