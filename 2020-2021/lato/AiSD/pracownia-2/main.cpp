#include <cmath>
#include <iostream>
#include <optional>
#include <utility>
#include <vector>
#include <limits>

void solve(const std::vector<int>& heights, size_t sum_of_heights) {
    std::vector<int> solutions(sum_of_heights + 1, -1);

    for (const auto height : heights) {
        const auto prev_solutions = solutions;
        for (size_t diff = 0; diff < sum_of_heights + 1; diff++) {
            if (prev_solutions[diff] != -1) {
                const auto smaller = prev_solutions[diff];
                const auto bigger = smaller + (int)diff;

                const auto new_from_smaller = smaller + height;
                const auto diff_from_smaller = std::abs(bigger - new_from_smaller);
                if (new_from_smaller > bigger) {
                    if (solutions[diff_from_smaller] < bigger) {
                        solutions[diff_from_smaller] = bigger;
                    }
                } else if (solutions[diff_from_smaller] < new_from_smaller ) {
                    solutions[diff_from_smaller] = new_from_smaller;
                }

                if (solutions[diff + height] < smaller) {
                    solutions[diff + height] = smaller;
                }
            } else if ((int)diff == height) {
                solutions[diff] = 0;
            }
        }
    }

    if (solutions[0] > 0) {
        std::cout << "TAK\n" << solutions[0] << std::endl;
    } else {
        for (size_t diff = 0; diff < sum_of_heights + 1; diff++) {
            if (solutions[diff] > 0) {
                std::cout << "NIE\n" << diff << std::endl;
                return;
            }
        }
    }
}

auto main() -> int {
    std::ios::sync_with_stdio(false);

    size_t num_of_bricks;
    std::cin >> num_of_bricks;
    size_t sum_of_heights = 0;

    std::vector<int> bricks(num_of_bricks, 0);
    for (auto& height : bricks) {
        std::cin >> height;
        sum_of_heights += height;
    }

    solve(bricks, sum_of_heights);
}
