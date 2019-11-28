#ifndef GRAPH_HPP
#define GRAPH_HPP

#include <map>
#include <string>
#include <list>
#include <initializer_list>
#include <tuple>
#include <optional>


class graph {

    struct edge {
        int to;
        float weight;

        edge() = delete;
    };

    int max_id = 0;

    std::map<std::string, int> vertex_ids;
    std::map<int, std::string> vertex_names;

    std::map<int, std::list<edge>> vertices;

    void assert_unique_vertex(const std::string&, int) const;
    void assert_vertex_id_exists(int) const;
    void assert_vertex_name_exists(const std::string&) const;
    void assert_has_edge(int, int);

    void internal_add_edge(int, int, float);
    void internal_remove_edge(int, int);
    void internal_change_weight(int, int, float);

public:

    using vertex = std::pair<int, const std::string&>;

    graph() = default;
    graph(graph&&) = default;
    graph(const graph&) = default;
    graph(
        std::initializer_list<vertex>,
        std::initializer_list<std::tuple<int, int, float>>
    );

    void add_vertex(const std::string&);
    void add_vertex(int, const std::string&);

    void remove_vertex(const std::string&);
    void remove_vertex(int);

    void add_edge(const std::string&, const std::string&, float);
    void add_edge(int, int, float);
    void add_edge(int, const std::string&, float);
    void add_edge(const std::string&, int, float);

    void remove_edge(const std::string&, const std::string&);
    void remove_edge(int, int);
    void remove_edge(int, const std::string&);
    void remove_edge(const std::string&, int);

    void change_edge_weight(const std::string&, const std::string&, float);
    void change_edge_weight(int, int, float);
    void change_edge_weight(const std::string&, int, float);
    void change_edge_weight(int, const std::string&, float);

    auto get_vertices() const -> std::list<vertex>;
    auto get_neighbours(int) const -> std::list<std::pair<vertex, float>>;

    auto has_path(int, int) const -> bool;
    auto find_path(int, int) const -> std::optional<std::list<int>>;

    auto get_vertex_name(int) const -> std::optional<std::string>;
    auto get_vertex_id(const std::string&) -> std::optional<int>;
};


#endif