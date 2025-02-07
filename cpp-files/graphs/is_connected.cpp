#include <unordered_map>
#include <map>
#include <unordered_set>
#include <string>

#include <vector>
#include <queue>

#include <iostream>
#include <fstream>

template <class T>
using Graph = std::unordered_map<T, std::unordered_set<T>>;

std::ostream& operator<<(std::ostream& os, const Graph<std::string>& graph) {
    for (const auto& [vertex, neighbours] : graph) {
        os << vertex << " -> ";
        for (const auto& neighbour : neighbours) {
            os << neighbour << " ";
        }
        os << std::endl;
    }
    return os;
}

bool is_connected_bfs(const Graph<std::string>& graph, const std::string& start, const std::string& dest) {
    if (start == dest)
        return true;
    
    std::queue<std::string> queue;
    std::unordered_set<std::string> visited;

    queue.push(start);
    visited.insert(start);
    
    while(!queue.empty()) {
        auto curr = queue.front();
        queue.pop();

        for (const std::string& neighbour: graph.at(curr))
        {
            if (neighbour == dest)
                return true;

            if (visited.find(neighbour) == visited.end()) {
                visited.insert(neighbour);
                queue.push(neighbour);
            }
        }
    }

    return false;
}

bool is_connected_dfs(const Graph<std::string>& graph, const std::string& start, const std::string& dest) {
    std::unordered_set<std::string> visited;
    auto helper = [&](auto&& self, const std::string& curr) {
        if (curr == dest)
            return true;

        if (visited.find(curr) != visited.end())
            return false;
        visited.insert(curr);

        for (const std::string& neighbour : graph.at(curr)) {
            if (self(self, neighbour))
                return true;
        }
        
        return false;
    };
    
    return helper(helper, start);
}

int main() {
    Graph<std::string> graph = {
        {"A", {"B", "C"}},
        {"B", {"A", "D"}},
        {"C", {"A", "D", "K"}},
        {"D", {"B", "C"}},
        {"E", {}},
        {"K", {"C"}}
    };

    std::cout << graph << std::endl;

    std::cout << "BFS Traversal" << std::endl;
    std::cout << "B to C: " << is_connected_bfs(graph, "B", "C") << std::endl;
    std::cout << "E to A: " << is_connected_bfs(graph, "E", "A") << std::endl;
    std::cout << "A to A: " << is_connected_bfs(graph, "A", "A") << std::endl;
    std::cout << "A to B: " << is_connected_bfs(graph, "A", "B") << std::endl;
    std::cout << "C to E: " << is_connected_bfs(graph, "C", "E") << std::endl;
    std::cout << "K to B: " << is_connected_bfs(graph, "K", "B") << std::endl;

    std::cout << "=======================" << std::endl;

    std::cout << "DFS Traversal" << std::endl;
    std::cout << "B to C: " << is_connected_dfs(graph, "B", "C") << std::endl;
    std::cout << "E to A: " << is_connected_dfs(graph, "E", "A") << std::endl;
    std::cout << "A to A: " << is_connected_dfs(graph, "A", "A") << std::endl;
    std::cout << "A to B: " << is_connected_dfs(graph, "A", "B") << std::endl;
    std::cout << "C to E: " << is_connected_dfs(graph, "C", "E") << std::endl;
    std::cout << "K to B: " << is_connected_dfs(graph, "K", "B") << std::endl;

    return 0;
}