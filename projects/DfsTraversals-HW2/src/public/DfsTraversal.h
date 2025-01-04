#pragma once

#include <fstream>
#include <vector>

#include "types.h"

struct DfsTraversal {
    std::vector<Vertex> order;

    std::vector<GraphEdge> treeEdges;
    std::vector<GraphEdge> backEdges;
    std::vector<GraphEdge> forwardEdges;
    std::vector<GraphEdge> crossEdges;

    bool operator<(const DfsTraversal &other) const;

    friend std::ostream &operator<<(std::ostream &os, const DfsTraversal &tr) noexcept;
};
