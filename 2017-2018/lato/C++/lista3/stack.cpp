#include "stack.hpp"

void stack::push(std::string element)
{
    if (this->m_size >= this->capacity)
    {
        this->capacity *= 2;
        std::string* temp_ptr = new std::string[this->capacity];
        
        for (int i = 0; i < this->m_size; i++)
            temp_ptr[i] = this->array[i];

        delete[] this->array;
        this->array = temp_ptr;
    }

    this->array[this->m_size] = element;
    this->m_size++;
}

auto stack::pop() -> std::string
{
    if (this->m_size)
    {
        this->m_size--;
        return this->array[this->m_size];
    }
    else
    {
        std::logic_error exception("pop(): empty stack");
        throw exception;
    }
}

auto stack::peek() const -> std::string
{
    if (this->m_size)
        return this->array[this->m_size - 1];
    else
    {
        std::logic_error exception("peek(): empty stack");
        throw exception;
    }
}

auto stack::size() const -> int
{
    return this->m_size;
}

auto stack::operator=(const stack& other) -> stack&
{
    if (&other != this)
    {
        if (this->capacity != other.capacity)
            delete[] this->array;

        this->m_size = other.m_size;
        this->capacity = other.capacity;

        this->array = new std::string[this->capacity];

        for (int i = 0; i < this->m_size; i++)
            this->array[i] = other.array[i];
    }

    return *this;
}

auto stack::operator=(stack&& other) ->stack&
{
    delete[] this->array;

    this->m_size = other.m_size;
    this->capacity = other.capacity;

    this->array = other.array;
    other.array = nullptr;
    other.m_size = 0;

    return *this;
}

stack::stack(int capacity)
{
    if (capacity < 0)
    {
        std::invalid_argument exception("stack(): negative initial capacity");
        throw exception;
    }

    this->capacity = capacity ? capacity : 1;
    this->m_size = 0;
    this->array = new std::string[this->capacity];
}

stack::stack(std::initializer_list<std::string> list) : stack(list.size())
{
    for (auto& str : list)
        this->push(str);
}

stack::stack(const stack& other)
{
    this->m_size = other.m_size;
    this->capacity = other.capacity;

    this->array = new std::string[this->capacity];

    for (int i = 0; i < this->m_size; i++)
        this->array[i] = other.array[i];
}

stack::stack(stack&& other)
{
    this->m_size = other.m_size;
    this->capacity = other.capacity;
    this->array = other.array;

    other.array = nullptr;
    other.m_size = 0;
}

stack::~stack()
{
    delete[] this->array;
}