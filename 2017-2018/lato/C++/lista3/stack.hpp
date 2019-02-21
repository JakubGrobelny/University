#ifndef stack_hpp
#define stack_hpp

#include <string>
#include <initializer_list>
#include <stdexcept>

class stack
{
    // std::string array
    std::string* array;

    // current maximum capacity of the stack
    int capacity;

    // number of elements on the stack
    int m_size;

public:

    // places the given element at the top of the stack
    void push(std::string element);

    // removes an element from the top and returns it
    auto pop() -> std::string;

    // checks what's on the top of the stack without removing it
    auto peek() const -> std::string;

    // returns the number of elments on the stack
    auto size() const -> int;


    // assignment operators:

    auto operator=(const stack& other) -> stack&;

    auto operator=(stack&& other) -> stack&;


    // constructors:

    stack(int capacity);

    stack(): stack(1) {};

    stack(std::initializer_list<std::string> list);

    stack(const stack& other);

    stack(stack&& other);

    // destructor
    ~stack();
};

#endif