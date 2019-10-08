#include <iostream>
#include <cstdint>


enum class name : uint16_t {
    Andrzej, 
    Jakub,
    Kacper,
    Klaudia,
    Karolina,
    Julia
};

void print_message(const std::string& message, name person) {

    const auto name_to_string = [](name person) -> const char* {
        switch (person)
        {
            case name::Andrzej:  return "Andrzej";
            case name::Jakub:    return "Jakub";
            case name::Kacper:   return "Kacper";
            case name::Klaudia:  return "Klaudia";
            case name::Karolina: return "Karolina";
            case name::Julia:    return "Julia";
            default:             return "";
        }
    };

    std::cout << name_to_string(person) << ", " << message << std::endl;
}


auto main(int argc, char* argv[]) -> int {
    print_message("Zrób zadania z C++", name::Kacper);
    return 0;
}


