#pragma once

#include <set>
#include <unordered_map>
#include <vector>

#include "DfsTraversal.h"
#include "types.h"

class DfsTraversalsCollection {
    using iterator = typename std::vector<DfsTraversal>::const_iterator;

   public:
    DfsTraversalsCollection(const Graph &graph, Vertex start);

    std::vector<EdgeVector> treeEdges() const noexcept;
    std::vector<EdgeVector> forwardEdges() const noexcept;
    std::vector<EdgeVector> backEdges() const noexcept;
    std::vector<EdgeVector> crossEdges() const noexcept;

    iterator begin() const noexcept;
    iterator end() const noexcept;

   private:
    const Graph &_graph;
    Vertex _startVertex;
    std::vector<DfsTraversal> _allTraversals;

    std::vector<EdgeVector> _treeEdges;
    std::vector<EdgeVector> _forwardEdges;
    std::vector<EdgeVector> _backEdges;
    std::vector<EdgeVector> _crossEdges;

    void _generateTraversals();

    void dfsVisit(const Graph &graph, const Vertex &u, std::unordered_map<Vertex, bool> &visited,
                  VertexTimestamps &timestamps, int &timeCounter, DfsTraversal &result);

    DfsTraversal runDfs(const Graph &graph, Vertex start);
};