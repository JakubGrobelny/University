#include <iostream>
#include <algorithm>
#include <functional>
#include <random>
#include <vector>
#include <list>
#include <set>
#include <iterator>


auto red(std::ostream& out) -> std::ostream& {
    out << "\u001b[31m";
    return out;
}


auto normal(std::ostream& out) -> std::ostream& {
    out << "\u001b[0m";
    return out;
}



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
    
    const size_t len = random<size_t>(1, max_len);
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


template <typename Collection>
void print_collection(const Collection& col) {
    for (auto& x : col)
        std::cout << x << ' ';
    std::cout << std::endl;
};


template <typename Collection>
auto sum (Collection xs, typename Collection::value_type initial) {
    std::for_each(xs.begin(), xs.end(), [&](auto& x) {
        initial += x;
    });

    return initial;
};


template <typename Collection>
auto minmax(const Collection& xs) {
    auto min = xs.begin();
    auto max = xs.begin();
    auto it = xs.begin();

    std::for_each(xs.begin(), xs.end(), [&](auto& x) {
        if (x < *min)
            min = it;
        else if (*max < x)
            max = it;
        it++;
    });

    return std::pair{min, max};
};


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

    std::cout << red << "vector<double>" << normal << ": " << std::endl;
    print_collection<std::vector<double>>(vec);
    std::cout << std::endl;

    std::cout << red << "set<int>" << normal << ": " << std::endl;
    print_collection<std::set<int>>(set);
    std::cout << std::endl;

    std::cout << red << "list<string>" << normal << ": " << std::endl;
    print_collection<std::list<std::string>>(list);
    std::cout << std::endl;

    // Zadanie 1
    std::cout << red << "Zadanie 1" << normal << ": " << std::endl;
    std::for_each(vec.begin(), vec.end(), make_comparator<int>(-10, 10));
    std::cout << std::endl;

    std::for_each(set.begin(), set.end(), make_comparator<double>(-10.0, 10.0));
    std::cout << std::endl;

    std::for_each(
        list.begin(), 
        list.end(), 
        make_comparator<std::string>("F", "M")
    );
    std::cout << std::endl << std::endl;


    // Zadanie 2
    constexpr size_t k = 3;
    constexpr size_t p = 14;
    
    std::cout << red << "Zadanie 2" << normal << ": " << std::endl;
    std::cout << "k = " << k << ", p = " << p << std::endl;

    std::for_each(vec.begin() + p, vec.end(), [](double x) {
        static size_t count = 0;
        if (count++ % k == 0)
            std::cout << x << ' ';
    });
    std::cout << std::endl << std::endl;

    auto set_it = set.begin();
    std::advance(set_it, p);
    std::for_each(set_it, set.end(), [](int x) {
        static size_t count = 0;
        if (count++ % k == 0)
            std::cout << x << ' ';
    });
    std::cout << std::endl << std::endl;

    auto list_it = list.begin();
    std::advance(list_it, p);
    std::for_each(list_it, list.end(), [](const std::string& str) {
        static size_t count = 0;
        if (count++ % k == 0)
            std::cout << str << ' ';
    });
    std::cout << std::endl << std::endl;

    // Zadanie 3
    std::cout << red << "Zadanie 3" << normal << ": " << std::endl;
    // auto vec_sum = std::accumulate(vec.begin(), vec.end(), 0.0);
    auto vec_sum = sum<std::vector<double>>(vec, 0.0);
    // auto set_sum = std::accumulate(set.begin(), set.end(), 0);
    auto set_sum = sum<std::set<int>>(set, 0);
    // auto list_sum = std::accumulate(list.begin(), list.end(), std::string());
    auto list_sum = sum<std::list<std::string>>(list, std::string());

    std::cout << "Średnia wektora: " 
              << vec_sum / vec.size()
              << std::endl;

    std::cout << "Średnia zbioru: " 
              << set_sum / static_cast<double>(set.size())
              << std::endl 
              << std::endl;

    // Zadanie 4
    std::cout << red << "Zadanie 4" << normal << ": " << std::endl;
    auto [vmin, vmax] = minmax<std::vector<double>>(vec);
    // auto [vmin, vmax] = std::minmax_element(vec.begin(), vec.end());
    auto [smin, smax] = minmax<std::set<int>>(set);
    // auto [smin, smax] = std::minmax_element(set.begin(), set.end());
    auto [lmin, lmax] = minmax<std::list<std::string>>(list);
    // auto [lmin, lmax] = std::minmax_element(list.begin(), list.end());

    std::cout << "Min wektora: " 
              << *vmin 
              << ", max wektora: " 
              << *vmax
              << std::endl;

    std::cout << "Min zbioru: " 
              << *smin 
              << ", max zbioru: " 
              << *smax
              << std::endl;

    std::cout << "Min listy: " 
              << *lmin 
              << ", max listy: " 
              << *lmax
              << std::endl
              << std::endl;

    // Zadanie 5
    std::cout << red << "Zadanie 5" << normal << ": " << std::endl;

    std::cout << "Suma wektora: " << vec_sum << std::endl;
    std::cout << "Suma zbioru: " << set_sum << std::endl;
    std::cout << "Suma listy: " << list_sum << std::endl;


    return EXIT_SUCCESS;
}
