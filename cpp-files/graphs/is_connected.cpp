#define DOCTEST_CONFIG_IMPLEMENT
#include <fstream>
#include <iostream>
#include <map>
#include <queue>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "../lib/doctest.h"

using namespace std;

template <class T>
using Graph = unordered_map<T, unordered_set<T>>;

ostream& operator<<(ostream& os, const Graph<string>& graph) {
    for (const auto& [vertex, neighbours] : graph) {
        os << vertex << " -> ";
        for (const auto& neighbour : neighbours) {
            os << neighbour << " ";
        }
        os << endl;
    }
    return os;
}

bool is_connected_bfs(const Graph<string>& graph, const string& start,
                      const string& dest) {
    if (start == dest) 
        return true;

    queue<string> queue;
    unordered_set<string> visited;

    queue.push(start);
    visited.insert(start);
    while (!queue.empty()) {
        const string curr = queue.front();
        queue.pop();

        for (const auto& neighbour : graph.at(curr)) {
            if (neighbour == dest)
                return true;
            
            if (visited.find(neighbour) == visited.end()) {
                queue.push(neighbour);
                visited.insert(neighbour);
            }
        }
    }

    return false;
}

bool is_connected_dfs(const Graph<string>& graph, const string& start,
                      const string& dest) {
    if (start == dest)
        return true;

    unordered_set<string> visited;
    auto helper = [&](auto&& self, string curr) -> bool {
        if (curr == dest)
            return true;

        if (visited.find(curr) != visited.end())
            return false;
        visited.insert(curr);

        for (const string& neighbour : graph.at(curr)) {
            if (self(self, neighbour))
                return true;
        }
        
        return false;
    };

    return helper(helper, start);
}

TEST_CASE("Graph traversals") {
    Graph<string> graph = {
        {"A", {"B", "C"}}, 
        {"B", {"A", "D"}}, 
        {"C", {"A", "D", "K"}},
        {"D", {"B", "C"}},
        {"E", {}}, 
        {"K", {"C"}}};
        
        CHECK(is_connected_bfs(graph, "B", "C"));
        CHECK(is_connected_dfs(graph, "B", "C"));

        CHECK(!is_connected_bfs(graph, "E", "A"));
        CHECK(!is_connected_dfs(graph, "E", "A"));
        
        CHECK(is_connected_bfs(graph, "A", "A"));
        CHECK(is_connected_dfs(graph, "A", "A"));
        
        CHECK(is_connected_bfs(graph, "A", "B"));
        CHECK(is_connected_dfs(graph, "A", "B"));
        
        CHECK(!is_connected_bfs(graph, "C", "E"));
        CHECK(!is_connected_dfs(graph, "C", "E"));
        
        CHECK(is_connected_bfs(graph, "K", "B"));
        CHECK(is_connected_dfs(graph, "K", "B"));
}

int main() {
    doctest::Context().run();
    return 0;
}