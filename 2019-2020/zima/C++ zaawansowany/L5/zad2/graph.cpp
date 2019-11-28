#include "graph.hpp"
#include <algorithm>
#include <sstream>


graph::graph(
    std::initializer_list<std::pair<int, const std::string&>> vertices, 
    std::initializer_list<std::tuple<int, int, float>> edges
) {
    for (auto& [id, name] : vertices) {
        this->vertex_ids[name] = id;
        this->vertex_names[id] = name;
        this->vertices.insert({id, {}});
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
    this->add_vertex(this->max_id++, name);
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

    for (auto& vertex : this->vertices) {
        std::remove_if(
            vertex.second.begin(), 
            vertex.second.end(), 
            [&](graph::edge e) -> bool {
                return e.to == id;
            }
        );
    }
}










