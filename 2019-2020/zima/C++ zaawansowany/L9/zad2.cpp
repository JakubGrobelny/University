#include <iostream>
#include <iterator>
#include <vector>
#include <algorithm>
#include <iomanip>
#include <fstream>


auto gcd(uint64_t a, uint64_t b) -> uint64_t {
    if (!b) {
        return a;
    } else {
        return gcd(b, a % b);
    }
}


auto totient(uint64_t k) -> uint64_t {
    uint64_t result = 0;

    for (uint64_t i = 1; i <= k; i++) {
        if (gcd(k, i) == 1) {
            result++;
        }
    }

    return result;
}


struct generator {
    int k = 1;
    int operator()() {
        return totient(this->k++);
    }
};


auto main() -> int {
    int k;
    std::cin >> k;
    
    std::vector<uint64_t> values(k);
    std::generate(values.begin(), values.end(), generator());

    std::ofstream file("phi.txt");
    std::ostream_iterator<uint64_t> out(file, "; ");

    std::for_each(values.rbegin(), values.rend(), [&](uint64_t n) {
        *out = n;
    });
}
