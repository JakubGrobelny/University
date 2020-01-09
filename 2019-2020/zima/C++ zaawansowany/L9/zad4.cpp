#include <filesystem>
#include <iostream>
#include <algorithm>


namespace fs = std::filesystem;

auto main(int argc, char* argv[]) -> int {
    for (int i = 1; i < argc; i++) {
        auto dir = fs::path(argv[i]);
        
        if (!fs::exists(dir)) {
            std::cerr << dir << " does not exist!" << std::endl;
            continue;
        } else if (!fs::is_directory(dir)) {
            std::cerr << dir << " is not a directory!" << std::endl;
            continue;
        }

        std::cout << fs::canonical(dir) << ": " << std::endl;
        for (auto& entry : fs::directory_iterator(dir)) {
            std::cout << "    " << entry.path().filename() << std::endl;
        }
    }
}
