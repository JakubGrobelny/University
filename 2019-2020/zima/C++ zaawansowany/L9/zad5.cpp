#include <filesystem>
#include <iostream>
#include <numeric>


namespace fs = std::filesystem;


// alternative solution:
auto directory_size(const fs::path& path) -> size_t {
    if (!fs::is_directory(path)) {
        return fs::file_size(path);
    }

    auto it_begin = fs::directory_iterator(path);
    auto it_end = fs::directory_iterator{};

    const auto f = [](size_t acc, const fs::directory_entry& entry) -> size_t {
        return acc + directory_size(entry.path());
    };
    
    return std::accumulate(it_begin, it_end, 0, f);
}

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

        size_t size = 0;
        for (auto& entry : fs::recursive_directory_iterator(dir)) {
            if (!fs::is_directory(entry.path())) {
                size += fs::file_size(entry.path());
            }
        }

        std::cout << fs::canonical(dir) << ": " << size << std::endl;
    }
}
