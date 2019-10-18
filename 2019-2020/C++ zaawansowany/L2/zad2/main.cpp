#include <iostream>
#include <fstream>
#include <string>
#include <memory>
#include <vector>


class line_writer {

    std::ofstream* file;

public:

    template <typename T>
    auto write(T data) -> line_writer& {
        *this->file << data << std::endl;
        return *this;
    }

    line_writer(const char* path, std::ios_base::openmode mode) {
        file = new std::ofstream(path, mode);

        if (!this->file->is_open()) {
            throw std::ofstream::failure("failed to open the file");
        }
    }

    line_writer(const std::string& path, std::ios_base::openmode mode) 
        : line_writer(path.c_str(), mode) {}

    ~line_writer() {
        this->file->close();
        delete this->file;
        std::cout << "[file closed]" << std::endl;
    }

    auto operator=(line_writer&&) -> line_writer& = delete;

    auto operator=(const line_writer&) -> line_writer& = delete;

    line_writer(line_writer&& writer) {
        this->file = writer.file;
        writer.file = nullptr;
    }
};


auto main(int argc, char* argv[]) -> int {

    if (argc < 2) {
        std::cerr << "Not enough arguments!" << std::endl;
        return EXIT_FAILURE;
    }

    auto writer = std::make_shared<line_writer>(argv[1], std::ios::out);

    std::vector<std::shared_ptr<line_writer>> pointers;

    for (int i = 2; i < argc; i++) {
        pointers.push_back(writer);
    }

    for (int i = 2; i < argc; i++) {
        pointers[i-2]->write(argv[i]);
    }

    {
        std::shared_ptr p0 = writer;
        p0->write("p0");

        std::shared_ptr p1 = p0;
        p1->write("p1");
    }

    std::shared_ptr p2 = writer;
    p2->write("p2");

    return EXIT_SUCCESS;

}
