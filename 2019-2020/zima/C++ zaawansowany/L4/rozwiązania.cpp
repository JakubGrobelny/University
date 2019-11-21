#include <iostream>
#include <algorithm>
#include <functional>
#include <random>
#include <vector>
#include <list>
#include <set>
#include <iterator>


template <typename T, typename Generator = std::mt19937_64>
auto random(T beginning, T end) -> T {
    static Generator gen(std::random_device{}());

    if constexpr(std::is_integral<T>::value) {
        return std::uniform_int_distribution<T>(beginning, end)(gen);
    } else {
        return std::uniform_real_distribution<T>(beginning, end)(gen);
    }
}


auto random_str(size_t max_len) -> std::string {
    static constexpr char characters[] = "qwertyuiopasdfghjklzxcvbnm"
                                         "QWERTYUIOPASDFGHJKLZXCVBNM"
                                         "1234567890";
    
    const size_t len = random<size_t>(0, max_len);
    std::string result;
    
    for (size_t i = 0; i < len; i++) {
        size_t index = random<size_t>(0, sizeof(characters) - 1);
        result.push_back(characters[index]);
    }

    return result;
}


template <typename T>
auto make_comparator(const T& a, const T& b) -> std::function<void(T)> {
    return [&](const T& x) {
        if (b > x && x > a)
            std::cout << x << ' ';
    };
}



auto main() -> int {
    constexpr size_t num = 42;
    
    std::vector<double> vec(num);
    std::generate(
        vec.begin(), 
        vec.end(), 
        [](){ return random<double>(-100.0, 100.0); }
    );

    std::set<int> set;
    for (size_t i = 0; i < num; i++)
        set.insert(random<int>(-100, 100));

    std::list<std::string> list;
    std::generate_n(
        std::back_inserter(list), 
        num, 
        [](){ return random_str(20); }
    );

    // Zadanie 1
    std::for_each(vec.begin(), vec.end(), make_comparator<int>(-10, 10));
    std::cout << std::endl;

    std::for_each(set.begin(), set.end(), make_comparator<double>(-10.0, 10.0));
    std::cout << std::endl;

    std::for_each(
        list.begin(), 
        list.end(), 
        make_comparator<std::string>("F", "M")
    );
    std::cout << std::endl;

    return EXIT_SUCCESS;
}









