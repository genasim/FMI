#pragma once

#include <fstream>
#include <set>
#include <unordered_map>

using Vertex = int;
using Neighbours = std::set<Vertex>;

using Graph = std::unordered_map<Vertex, Neighbours>;

using GraphEdge = std::pair<Vertex, Vertex>;
std::ostream &operator<<(std::ostream &os, const GraphEdge &edge);

struct VertexTimestamps {
    std::unordered_map<Vertex, int> discoverTime;
    std::unordered_map<Vertex, int> finishTime;
};

using EdgeVector = std::vector<GraphEdge>;
