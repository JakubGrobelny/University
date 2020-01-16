#include <iostream>
#include <fstream>
#include <algorithm>
#include <list>
#include <utility>
#include <functional>


struct point {
    std::string name;
    double x  = 0;
    double y  = 0;
    uint8_t r = 0;
    uint8_t g = 0;
    uint8_t b = 0;
};


auto operator>> (std::istream& in, point& dest) -> std::istream& {
    in >> dest.x >> dest.y;

    int r, g, b;
    in >> r >> g >> b;
    dest.r = r;
    dest.g = g;
    dest.b = b;

    in >> dest.name;

    return in;
}


auto operator<< (std::ostream& out, const point& point) -> std::ostream& {
    out << '"' << point.name << "\": {"
        << "x: " << point.x << ", "
        << "y: " << point.y << ", "
        << "RGB: (" << +point.r << ", " << +point.g << ", " << +point.b << ")}";

    return out;
}


auto read_data(const char* filename) -> std::list<point> {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Can't open file '" << filename << "'!" << std::endl;
        exit(EXIT_FAILURE);
    }

    std::list<point> output;
    while (!file.eof()) {
        point p;
        file >> p;
        output.push_back(p);
    }

    return output;
}


template <typename Container, typename Predicate>
auto filter(const Container& c, Predicate p) -> Container {
    Container filtered;
    std::copy_if(c.begin(), c.end(), std::back_inserter(filtered), p);
    return filtered;
}


auto main(int argc, char* argv[]) -> int {
    if (argc != 2) {
        std::cerr << "Usage: [filename]" << std::endl;
        return EXIT_FAILURE;
    }

    auto data = read_data(argv[1]);

    std::pair<int, int> quadrant_signs[4] = {
        {1, 1}, {-1, 1}, {-1, -1}, {1, -1}
    };

    const char* quadrant_names[4] = {
        "I", "II", "III", "IV"
    };

    for (size_t i = 0; i < 4; i++) {
        std::cout << quadrant_names[i] << ": " << std::endl;
        auto points = filter(data, [&](const point& p) -> bool {
            return p.x * quadrant_signs[i].first > 0 &&
                   p.y * quadrant_signs[i].second > 0;
        });

        for (auto& point : points) {
            std::cout << point << std::endl;
        }

        std::cout << std::endl;
    }

    auto brightest_darkest = std::minmax_element(
        data.begin(), 
        data.end(), 
        [](const point& p1, const point& p2) {
            double avg_p1 = (p1.r + p1.g + p1.b) / 3;
            double avg_p2 = (p2.r + p2.g + p2.b) / 3;
            return avg_p1 > avg_p2;
        }
    );

    std::cout << "Brightest: " << *brightest_darkest.first << std::endl;

    std::cout << "Darkest: " << *brightest_darkest.second << std::endl;

}