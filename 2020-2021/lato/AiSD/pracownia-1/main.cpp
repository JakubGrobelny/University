#include <algorithm>
#include <iostream>
#include <vector>

using iterator = std::vector<std::pair<uint64_t, size_t>>::iterator;

const auto length_comparator = [](const auto& p1, const auto& p2) {
    return p1.first < p2.first;
};

auto read_input() -> std::vector<std::pair<uint64_t, size_t>> {
    size_t size;
    std::cin >> size;

    std::vector<std::pair<uint64_t, size_t>> input;
    for (size_t i = 0; i < size; i++) {
        uint64_t length;
        size_t count;

        std::cin >> length >> count;
        input.emplace_back(length, count);
    }

    return input;
}

void sort_input(std::vector<std::pair<uint64_t, size_t>>& input) {
    std::sort(input.begin(), input.end(), length_comparator);
}

auto reduce_strings(iterator begin, iterator end) -> size_t {
    size_t discarded_strings = 0;
    auto pair = *begin++;
    auto& [length, count] = pair;

    while (count != 0) {
        const auto& it = std::lower_bound(begin, end, pair, length_comparator);
        if (it != end && (*it).first == length) {
            (*it).second += count;
            break;
        }

        if (count % 2 == 1) {
            discarded_strings++;
        }

        count /= 2;
        length *= 2;
    }

    return discarded_strings;
}

auto main() -> int {
    std::ios::sync_with_stdio(false);

    auto input = read_input();
    sort_input(input);

    size_t final_count = 0;

    for (auto it = input.begin(); it != input.end(); ++it) {
        final_count += reduce_strings(it, input.end());
    }

    std::cout << final_count << std::endl;
}
