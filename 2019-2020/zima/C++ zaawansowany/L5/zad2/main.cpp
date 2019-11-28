#include <iostream>
#include "graph.hpp"


void print_graph(const graph& g) {
    for (auto& [id, name] : g.get_vertices()) {
        std::cout << '[' << id << "] '" << name << "': " << std::endl;

        for (auto& [nvertex, nweight] : g.get_neighbours(id)) {
            std::cout << '\t' 
                      << '[' 
                      << nvertex.first 
                      << "] '" 
                      << nvertex.second
                      << "', weight: "
                      << nweight
                      << std::endl;
        }
    }
}


void find_and_print_path(const graph& g, int from, int to) {
    auto path = g.find_path(from, to);

    if (path.has_value()) {
        for (auto& v : *path) {
            std::cout << v << (v == to ? "" :  " -> ");
        }
        std::cout << std::endl;
    } else {
        std::cout << std::endl 
                  << "No path between " 
                  << from 
                  << " and " 
                  << to 
                  << "!" 
                  << std::endl;
    }
}


auto main(void) -> int {
    auto g = graph(
        {
            {1, std::string("jeden")},
            {2, std::string("dwa")},
            {3, std::string("trzy")},
            {4, std::string("cztery")},
            {5, std::string("pięć")}
        }, 

        {
            {1, 2, 1.0 },
            {2, 3, 1.0 },
            {3, 4, 1.0 },
            {4, 5, 3.14},
            {2, 4, 0.5 },
            {5, 2, 0.75}
        }
    );

    print_graph(g);
    std::cout << std::endl << "Po dodaniu wierzchołków: " << std::endl;

    g.add_edge(3, 1, 10.0);
    g.add_vertex("sześć?");
    g.add_vertex("siedem?");
    g.add_edge(6, 7, 6.7);
    g.change_edge_weight(3, 4, 34.12);

    print_graph(g);

    find_and_print_path(g, 1, 5);
    find_and_print_path(g, 1, 7);

    std::cout << std::endl << "Added edge 4 -> 6" << std::endl;
    g.add_edge(4, 6, 0.2);
    find_and_print_path(g, 1, 7);

    std::cout << std::endl << "Removed and changed edges" << std::endl;
    g.change_edge_weight("sześć?", 7, 1000.0);
    g.change_edge_weight(2, "trzy", 0.1);
    g.remove_edge("jeden", 2);
    g.remove_edge(3, "cztery");
    print_graph(g);
    find_and_print_path(g, 1, 5);

    std::cout << std::endl << "Removed some vertices" << std::endl;
    g.remove_vertex(3);
    g.remove_vertex("dwa");
    g.remove_vertex("sześć?");
    print_graph(g);
    find_and_print_path(g, 1, 5);

    std::cout << std::endl << "Added some more vertices" << std::endl;
    g.add_edge("jeden", 7, 1.7);
    g.add_vertex("nowy");
    g.add_edge("nowy", 5, 5.5);
    print_graph(g);

    return EXIT_SUCCESS;
}