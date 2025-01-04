#include "DfsTraversal.h"

#include <iostream>

bool DfsTraversal::operator<(const DfsTraversal& other) const {
    return std::lexicographical_compare(this->order.begin(), this->order.end(), other.order.begin(),
                                        other.order.end());
}

std::ostream& operator<<(std::ostream& os, const DfsTraversal& tr) noexcept {
    os << "Order: ";
    for (const Vertex& vertex : tr.order) os << vertex << ' ';
    os << std::endl;

    os << "   Tree edges: ";
    for (const GraphEdge& edge : tr.treeEdges) os << edge << ' ';
    os << std::endl;

    os << "   Back edges: ";
    for (const GraphEdge& edge : tr.backEdges) os << edge << ' ';
    os << std::endl;

    os << "   Forward edges: ";
    for (const GraphEdge& edge : tr.forwardEdges) os << edge << ' ';
    os << std::endl;

    os << "   Cross edges: ";
    for (const GraphEdge& edge : tr.crossEdges) os << edge << ' ';
    os << std::endl;

    return os;
}

std::ostream& operator<<(std::ostream& os, const GraphEdge& edge) {
    os << "(" << edge.first << "->" << edge.second << ")";
    return os;
}
