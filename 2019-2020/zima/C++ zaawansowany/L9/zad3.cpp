#include <filesystem>
#include <iostream>
#include <algorithm>
#include <iterator>
#include <chrono>

namespace fs = std::filesystem;
namespace chr = std::chrono;


auto file_type_string(const fs::path& path) -> const char* {
    switch (fs::status(path).type()) {
        case fs::file_type::regular:
            return "file";
        case fs::file_type::directory:
            return "directory";
        case fs::file_type::symlink:
            return "symlink";
        case fs::file_type::block:
        case fs::file_type::character:
            return "device";
        case fs::file_type::fifo:
            return "fifo";
        case fs::file_type::socket:
            return "socket";
        default:
            return nullptr;
    }
}


// ???!
template <typename T>
auto convert_time(const T& time) -> std::time_t {
    return chr::system_clock::to_time_t(
        chr::time_point_cast<chr::system_clock::duration>(
            time - T::clock::now() + chr::system_clock::now()
        )
    );
}


auto main(int argc, char* argv[]) -> int {
    for (int i = 1; i < argc; i++) {
        if (fs::exists(fs::path(argv[i]))) {
            auto path = fs::path(argv[i]);
            
            std::cout << fs::canonical(path) << " | ";
            std::cout << file_type_string(path) << " | ";

            if (fs::is_regular_file(path)) {
                std::cout << fs::file_size(path) << " bytes | ";
            }

            // NIE DZIAŁA
            // https://en.cppreference.com/w/cpp/filesystem/last_write_time
            /*
            auto ftime = fs::last_write_time(path);
            std::time_t ctime = decltype(ftime)::clock::to_time_t(ftime);
            std::cout << std::asctime(std::localtime(&ctime)) << ' ';
            */

            auto time = fs::last_write_time(path);
            std::time_t ctime = convert_time(time);
            std::cout << std::asctime(std::localtime(&ctime));
        } else {
            std::cout << '"' << argv[i] << '"' 
                      << " does not exist" 
                      << std::endl;
        }
    }
}
