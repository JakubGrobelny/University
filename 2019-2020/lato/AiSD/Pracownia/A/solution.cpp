#include <iostream>
#include <array>
#include <stack>
#include <vector>

struct vertex {
    uint16_t left   = 0;
    uint16_t length = 0;
    uint16_t right  = 0;
    bool visited    = false;

    vertex() = default;
};

auto operator<< (std::ostream& out, vertex& brick) -> std::ostream& {
    out << brick.left << ' ' << brick.length << ' ' << brick.right;
    return out;
}

auto operator>> (std::istream& in, vertex& brick) -> std::istream& {
    in >> brick.left >> brick.length >> brick.right;
    return in;
}

using graph = std::array<std::vector<vertex>, 10001>;
using solution = std::stack<vertex>;

auto find_path(graph& bricks, uint16_t left, solution& stack) -> bool {
    static std::array<bool, 10001> visited = {false};

    if (visited[left]) {
        return false;
    }

    for (auto& brick : bricks[left]) {
        if (brick.visited) {
            continue;
        }

        brick.visited = true;

        if (brick.right == 0 || find_path(bricks, brick.right, stack)) {
            stack.push(brick);
            return true;
        }
    }

    visited[left] = true;
    return false;
}

auto main() -> int {
    std::ios::sync_with_stdio(false);

    int n;
    std::cin >> n;

    graph bricks;

    for (int i = 0; i < n; i++) {
        vertex input;
        std::cin >> input;
        bricks[input.left].push_back(input);
    }

    solution stack;
    if (!find_path(bricks, 0, stack)) {
        std::cout << "BRAK" << '\n';
    } else {
        std::cout << stack.size() << '\n';
        while (!stack.empty()) {
            std::cout << stack.top() << '\n';
            stack.pop();
        }
    }
}