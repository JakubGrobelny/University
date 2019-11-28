#include "graph.hpp"
#include <algorithm>
#include <sstream>
#include <queue>
#include <set>
#include <iostream>


graph::graph(
    std::initializer_list<std::pair<int, const std::string&>> vertices, 
    std::initializer_list<std::tuple<int, int, float>> edges
) {
    for (auto& [id, name] : vertices) {
        this->vertex_ids[name] = id;
        this->vertex_names[id] = name;
        this->vertices.insert({id, {}});
        if (id > this->max_id)
            this->max_id = id;
    }

    for (auto& [from, to, weight] : edges) {
        this->add_edge(from, to, weight);
    }
}


void graph::assert_unique_vertex(const std::string& name, int id) const {
    if (this->vertex_names.find(id) != this->vertex_names.end()) {
        std::stringstream error_msg;
        error_msg << "Vertex with id [" << id << "] already exists!";
        throw std::invalid_argument(error_msg.str());
    }

    if (this->vertex_ids.find(name) != this->vertex_ids.end()) {
        std::stringstream error_msg;
        error_msg << "Vertex with name '" << name << "' already exists!";
        throw std::invalid_argument(error_msg.str());
    }
}

void graph::assert_vertex_id_exists(int id) const {
    if (this->vertices.find(id) == this->vertices.end()) {
        std::stringstream error_msg;
        error_msg << "Vertex with id [" << id << "] does not exist!";
        throw std::invalid_argument(error_msg.str());
    }
}

void graph::assert_vertex_name_exists(const std::string& name) const {
    if (this->vertex_ids.find(name) == this->vertex_ids.end()) {
        std::stringstream error_msg;
        error_msg << "Vertex with name '" << name << "' does not exist!";
        throw std::invalid_argument(error_msg.str());
    }
}


void graph::add_vertex(const std::string& name) {
    this->add_vertex(++this->max_id, name);
}


void graph::add_vertex(int new_id, const std::string& new_name) {
    this->assert_unique_vertex(new_name, new_id);

    this->vertex_ids[new_name] = new_id;
    this->vertex_names[new_id] = new_name;

    if (new_id > this->max_id) {
        this->max_id = new_id;
    }

    this->vertices.insert({new_id, {}});
}


void graph::remove_vertex(const std::string& name) {
    this->assert_vertex_name_exists(name);
    int id = this->vertex_ids[name];
    this->remove_vertex(id);
}


void graph::remove_vertex(int id) {
    this->assert_vertex_id_exists(id);

    this->vertex_ids.erase(this->vertex_names[id]);
    this->vertex_names.erase(id);
    this->vertices.erase(id);

    // https://en.wikipedia.org/wiki/Erase%E2%80%93remove_idiom
    // ??!
    for (auto& vertex : this->vertices) {
        vertex.second.erase(
            std::remove_if(
                vertex.second.begin(), 
                vertex.second.end(), 
                [&](graph::edge e) -> bool {
                    return e.to == id;
                }
            ),
            vertex.second.end()
        );
    }
}


void graph::add_edge(const std::string& v1, const std::string& v2, float w) {
    this->assert_vertex_name_exists(v1);
    this->assert_vertex_name_exists(v2);
    this->internal_add_edge(this->vertex_ids[v1], this->vertex_ids[v2], w);
}


void graph::add_edge(const std::string& v1, int v2, float w) {
    this->assert_vertex_name_exists(v1);
    this->assert_vertex_id_exists(v2);
    this->internal_add_edge(this->vertex_ids[v1], v2, w);
}


void graph::add_edge(int v1, const std::string& v2, float w) {
    this->add_edge(v2, v1, w);
}


void graph::add_edge(int v1, int v2, float w) {
    this->assert_vertex_id_exists(v1);
    this->assert_vertex_id_exists(v2);
    this->internal_add_edge(v1, v2, w);
}


void graph::internal_add_edge(int v1, int v2, float weight) {
    if (this->has_edge(v1, v2)) {
        std::stringstream error_msg;
        error_msg << "Graph already has an edge <" << v1 << ", " << v2 << ">!";
        throw std::invalid_argument(error_msg.str());
    }

    this->vertices[v1].push_front({v2, weight});
    this->vertices[v2].push_front({v1, weight});
}


void graph::remove_edge(const std::string& v1, const std::string& v2) {
    this->assert_vertex_name_exists(v1);
    this->assert_vertex_name_exists(v2);
    this->internal_remove_edge(this->vertex_ids[v1], this->vertex_ids[v2]);
}


void graph::remove_edge(const std::string& v1, int v2) {
    this->assert_vertex_name_exists(v1);
    this->assert_vertex_id_exists(v2);
    this->internal_remove_edge(this->vertex_ids[v1], v2);
}


void graph::remove_edge(int v1, const std::string& v2) {
    this->remove_edge(v2, v1);
}


void graph::remove_edge(int v1, int v2) {
    this->internal_remove_edge(v1, v2);
}


auto graph::has_edge(int v1, int v2) const -> bool {
    const auto& v1_edges = this->vertices.find(v1)->second;
    const auto& v2_edges = this->vertices.find(v2)->second;

    bool v1_edge_exists = std::any_of(
        v1_edges.begin(), 
        v1_edges.end(), 
        [&](graph::edge e) -> bool {
            return e.to == v2;
        }
    );

    bool v2_edge_exists = std::any_of(
        v2_edges.begin(), 
        v2_edges.end(), 
        [&](graph::edge e) -> bool {
            return e.to == v1;
        }
    );

    return v1_edge_exists && v2_edge_exists;
}



void graph::assert_has_edge(int v1, int v2) const {
    if (!this->has_edge(v1, v2)) {
        std::stringstream error_msg;
        error_msg << "Edge <" << v1 << ", " << v2 << "> does not exist!";
        throw std::invalid_argument(error_msg.str());
    }
}


void graph::internal_remove_edge(int v1, int v2) {
    this->assert_has_edge(v1, v2);

    this->vertices[v1].remove_if([&](graph::edge e) { return e.to == v2; });
    this->vertices[v2].remove_if([&](graph::edge e) { return e.to == v1; });
}


void graph::change_edge_weight(
    const std::string& v1, 
    const std::string& v2, 
    float weight
) {
    this->assert_vertex_name_exists(v1);
    this->assert_vertex_name_exists(v2);
    this->internal_change_weight(
        this->vertex_ids[v1], 
        this->vertex_ids[v2], 
        weight
    );
}


void graph::change_edge_weight(const std::string& v1, int v2, float weight) {
    this->assert_vertex_name_exists(v1);
    this->assert_vertex_id_exists(v2);
    this->internal_change_weight(this->vertex_ids[v1], v2, weight);
}


void graph::change_edge_weight(int v1, const std::string& v2, float weight) {
    this->assert_vertex_id_exists(v1);
    this->assert_vertex_name_exists(v2);
    this->internal_change_weight(v1, this->vertex_ids[v2], weight);
}


void graph::change_edge_weight(int v1, int v2, float weight) {
    this->assert_vertex_id_exists(v1);
    this->assert_vertex_id_exists(v2);
    this->internal_change_weight(v1, v2, weight);
}


void graph::internal_change_weight(int v1, int v2, float weight) {
    this->assert_has_edge(v1, v2);

    for (auto& neighbour : this->vertices[v1]) {
        if (neighbour.to == v2) {
            neighbour.weight = weight;
            break;
        }
    }

    for (auto& neighbour : this->vertices[v2]) {
        if (neighbour.to == v1) {
            neighbour.weight = weight;
            break;
        }
    }
}


auto graph::get_vertices() const -> std::list<vertex> {
    std::list<graph::vertex> vertices;

    for (auto& vertex : this->vertices) {
        auto& id = vertex.first;
        auto& name = this->vertex_names.find(id)->second;
        vertices.push_front({id, name});
    }

    return vertices;
}


auto graph::get_neighbours(int v) const -> std::list<std::pair<vertex, float>> {
    this->assert_vertex_id_exists(v);

    auto& neighbours = this->vertices.find(v)->second;
    std::list<std::pair<vertex, float>> result;

    for (auto& neighbour : neighbours) {
        int id = neighbour.to;
        auto& name = this->vertex_names.find(id)->second;
        float weight = neighbour.weight;

        result.push_front({{id, name}, weight});
    }

    return result;
}


auto graph::get_vertex_name(int id) const -> std::optional<std::string> {
    const auto& it = this->vertex_names.find(id);
    if (it == this->vertex_names.end()) {
        return {};
    }

    return it->second;
}


auto graph::get_vertex_id(const std::string& name) -> std::optional<int> {
    const auto& it = this->vertex_ids.find(name);
    if (it == this->vertex_ids.end()) {
        return {};
    }

    return it->second;
}


auto graph::has_path(int from, int to) const -> bool {
    return this->find_path(from, to).has_value();
}


auto graph::find_path(int from, int to) const -> std::optional<std::list<int>> {
    this->assert_vertex_id_exists(from);
    this->assert_vertex_id_exists(to);

    std::queue<int> queue;
    std::set<int> visited;
    std::map<int, int> parents;

    visited.insert(from);
    queue.push(from);

    const auto backtrack = [&](int child) -> std::list<int> {
        std::list<int> path;

        while (child != parents[child]) {
            path.push_front(child);
            child = parents[child];
        }

        path.push_front(from);

        return path;
    };

    parents[from] = from;

    while (!queue.empty()) {
        int v = queue.front();
        queue.pop();

        if (v == to) {
            return backtrack(v);
        }

        auto& neighbours = this->vertices.find(v)->second;
        for (auto& w : neighbours) {
            auto id = w.to;
            if (!visited.count(id)) {
                visited.insert(id);
                parents[id] = v;
                queue.push(id);
            }
        }
    }

    return {};
}