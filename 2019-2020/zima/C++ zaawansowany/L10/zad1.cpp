#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <iomanip>


struct person {
    std::string name;
    std::string surname;
    int age;
    int height;
    int weight;

    person(): age(0), height(0), weight(0) {};

    auto bmi() const -> double {
        double height_meters = this->height / 100.0;
        return this->weight / (height_meters * height_meters);
    }
};


auto operator>> (std::istream& in, person& dest) -> std::istream& {
    in >> dest.name;
    in >> dest.surname;
    in >> dest.age;
    in >> dest.height;
    in >> dest.weight;
    return in;
}


auto operator<< (std::ostream& out, const person& person) -> std::ostream& {
    std::cout << person.name << ' ' << person.surname << ", "
              << person.age << " years old, "
              << person.height << "cm, "
              << person.weight << "kg";
    return out;
}


auto read_data(const char* filename) -> std::vector<person> {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Can't open file '" << filename << "'!" << std::endl;
        exit(EXIT_FAILURE);
    }

    std::vector<person> output;
    while (!file.eof()) {
        person record;
        file >> record;
        output.push_back(record);
    }
    return output;
}


auto main(int argc, char* argv[]) -> int {
    if (argc < 2) {
        std::cerr << "Usage: [filename]" << std::endl;
        return EXIT_FAILURE;
    }

    auto data = read_data(argv[1]);

    std::sort(data.begin(), data.end(), [](const person& p1, const person& p2) {
        return p1.bmi() < p2.bmi();
    });

    std::cout << std::setprecision(1) << std::fixed;

    for (auto& person : data) {
        std::cout << "[BMI: " << person.bmi() << "] " << person << std::endl;
    }

    const person& oldest = *std::max_element(
        data.begin(), 
        data.end(), 
        [](const person& p1, const person& p2) {
            return p1.age < p2.age;
        }
    );

    const person& heaviest = *std::max_element(
        data.begin(),
        data.end(),
        [](const person& p1, const person& p2) {
            return p1.weight < p2.weight;
        }
    );

    const person& tallest = *std::max_element(
        data.begin(),
        data.end(),
        [](const person& p1, const person& p2) {
            return p1.height < p2.height;
        }
    );

    std::cout << std::endl;
    std::cout << "The oldest person: " << oldest << std::endl;
    std::cout << "The heaviest person: " << heaviest << std::endl;
    std::cout << "The tallest person: " << tallest << std::endl;
}