#include <iostream>
#include <utility>
#include <string>


template <
    typename From,
    typename To,
    typename std::enable_if<
        std::is_pointer<From>::value,
        void
    >::type* = nullptr,
    typename std::enable_if<
        std::is_convertible<
            typename std::remove_pointer<From>::type,
            To
        >::value,
        void
    >::type* = nullptr
>
auto move_from_to(From from, To& to) -> To& {
    to = std::move((To)*from);
    std::cout << "Used pointer version" << std::endl;
    return to;
}

template <
    typename From, 
    typename To,
    typename std::enable_if<
        std::is_convertible<From, To>::value,
        void
    >::type* = nullptr
>
auto move_from_to(From from, To& to) -> To& {
    to = std::move((To)from);
    std::cout << "Used normal version" << std::endl;
    return to;
}



auto main() -> int {
    std::string s0 = "abc123";
    std::string s1 = "qwe890";
    std::string s2;
    std::string* s3 = new std::string("123abc");

    move_from_to(s1, s2);
    std::cout << s2 << std::endl;

    move_from_to(s3, s0);
    std::cout << s0 << std::endl;

    delete s3;
}

