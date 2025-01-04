#include "DfsTraversalsCollection.h"

using namespace std;

DfsTraversalsCollection::DfsTraversalsCollection(const Graph& graph, Vertex start)
    : _graph(graph), _startVertex(start) {
    _generateTraversals();
}

inline vector<EdgeVector> DfsTraversalsCollection::treeEdges() const noexcept { return _treeEdges; }

inline vector<EdgeVector> DfsTraversalsCollection::forwardEdges() const noexcept {
    return _forwardEdges;
}

inline vector<EdgeVector> DfsTraversalsCollection::backEdges() const noexcept { return _backEdges; }

inline vector<EdgeVector> DfsTraversalsCollection::crossEdges() const noexcept {
    return _crossEdges;
}

typename DfsTraversalsCollection::iterator DfsTraversalsCollection::begin() const noexcept {
    return _allTraversals.cbegin();
}

typename DfsTraversalsCollection::iterator DfsTraversalsCollection::end() const noexcept {
    return _allTraversals.cend();
}

void DfsTraversalsCollection::_generateTraversals() {
    // generate all permutations

    auto traversal = runDfs(_graph, _startVertex);
    _allTraversals.push_back(traversal);
}

void DfsTraversalsCollection::dfsVisit(const Graph& graph, const Vertex& u,
                                       std::unordered_map<Vertex, bool>& visited,
                                       VertexTimestamps& timestamps, int& timeCounter,
                                       DfsTraversal& result) {
    auto& [discoverTime, finishTime] = timestamps;

    visited[u] = true;
    discoverTime[u] = ++timeCounter;

    result.order.push_back(u);

    for (const auto& v : graph.at(u)) {
        if (!visited[v]) {
            result.treeEdges.emplace_back(u, v);

            dfsVisit(graph, v, visited, timestamps, timeCounter, result);
            continue;
        }

        if (finishTime[v] == 0) {
            result.backEdges.emplace_back(u, v);
            continue;
        }

        if (discoverTime[u] < discoverTime[v] && finishTime[u] < finishTime[v]) {
            result.forwardEdges.emplace_back(u, v);
            continue;
        }

        result.crossEdges.emplace_back(u, v);
    }

    finishTime[u] = ++timeCounter;
}

DfsTraversal DfsTraversalsCollection::runDfs(const Graph& graph, Vertex start) {
    DfsTraversal result;

    std::unordered_map<Vertex, bool> visited;
    VertexTimestamps timestamps;
    std::unordered_map<Vertex, int> discoverTime;
    std::unordered_map<Vertex, int> finishTime;

    for (auto& kv : graph) {
        Vertex v = kv.first;
        visited[v] = false;
        timestamps.discoverTime[v] = 0;
        timestamps.finishTime[v] = 0;
    }

    int timeCounter = 0;
    dfsVisit(graph, start, visited, timestamps, timeCounter, result);

    return result;
}
