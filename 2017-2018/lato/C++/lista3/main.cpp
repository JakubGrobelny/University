#include <iostream>
#include <utility>
#include <cstdlib>

#include "stack.hpp"

void stack_initialization_menu(stack*& s)
{
    int option = -1;

    while (option < 1 || option > 5)
    {
        std::cout << "How do you want to initialize the stack?" << std::endl;
        std::cout << "  1) By using default constructor" << std::endl;
        std::cout << "  2) By entering the initial capacity" << std::endl;
        std::cout << "  3) By copying another stack" << std::endl;
        std::cout << "  4) By using move constructor" << std::endl;
        std::cout << "  5) By using initializer list" << std::endl;
        std::cout << "Enter the number corresponding to choosen option: ";

        std::cin >> option;

        std::system("clear");
    }

    switch (option)
    {
        case 1:
            s = new stack();
            break;
        case 2:
        {
            int initial_capacity = -1;
            std::cout << "Enter the initial capacity: ";
            while (initial_capacity < 0)
            {
                std::cin >> initial_capacity;
            }
            s = new stack(initial_capacity);

            break;
        }
        case 3:
        {
            stack temp_stack({"first", "second", "third"});
            s = new stack(temp_stack);

            std::cout << "Copied elements from stack containing ";
            while (temp_stack.size())
                std::cout << temp_stack.pop() << " ";
            std::cout << std::endl;

            break;
        }
        case 4:
        {
            stack temp_stack({"one", "two", "three"});

            s = new stack(std::move(temp_stack));

            std::cout << "Initialized stack using move constructor on stack containing one two three " << std::endl;

            break;
        }
        case 5:
        {
            std::string temp_array[3];

            std::cout << "Enter three words separated by space: ";

            for (int i = 0; i < 3; i++)
                std::cin >> temp_array[i];

            s = new stack({temp_array[0], temp_array[1], temp_array[2]});

            std::cout << "Initialized the stack with std::initializer_list" << std::endl;
            break;

        }
    }
}

void print_instructions()
{
    std::cout << " ______________________________" << std::endl;
    std::cout << " |      AVALIABLE OPTIONS     |" << std::endl;
    std::cout << " |  exit - leave the program  |" << std::endl;
    std::cout << " |  push arg - adds 'arg'     |" << std::endl;
    std::cout << " |   to the stack             |" << std::endl;
    std::cout << " |  pop - prints and removes  |" << std::endl;
    std::cout << " |   last word from the stack |" << std::endl;
    std::cout << " |  peek - prints the last    |" << std::endl;
    std::cout << " |   word from the stack      |" << std::endl;
    std::cout << " |  size - prints the size    |" << std::endl;
    std::cout << " ______________________________" << std::endl << std::endl << std::endl;;
}

void parse_input(const std::string& line, stack*& s)
{
    if (line.empty())
        return;
    
    std::string command;

    std::cout << "> ";

    unsigned int i = 0;

    while (line[i] != ' ' && i < line.length())
    {
        command.push_back(line[i]);
        i++;
    }
    
    if (command == "pop")
    {
        try
        {
            std::cout << s->pop() << std::endl;
        }
        catch(std::logic_error e)
        {
            std::cout << "Stack is empty!" << std::endl;
        }
    }
    else if (command == "peek")
    {
        try
        {
            std::cout << s->peek() << std::endl;
        }
        catch(std::logic_error e)
        {
            std::cout << "Stack is empty!" << std::endl;
        }
    }
    else if (command == "size")
    {
        std::cout << s->size() << std::endl;
    }
    else
    {
        std::string arg;

        i++;

        while (i < line.length())
        {
            arg.push_back(line[i]);
            i++;
        }

        s->push(arg);
        std::cout << "added " << arg << std::endl;
    }
}

auto main() -> int
{
    stack* s;
    stack_initialization_menu(s);

    std::string line;

    print_instructions();

    while (line != "exit")
    {
        parse_input(line, s);
        std::getline(std::cin, line);
    }

    delete s;
    return 0;
}