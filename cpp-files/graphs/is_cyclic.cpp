#define DOCTEST_CONFIG_IMPLEMENT

#include <map>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <functional>
#include <queue>

#include "../lib/doctest.h"

using namespace std;

template <class Node>
using Graph = unordered_map<Node, unordered_set<Node>>;

template <class T>
bool is_cyclic_directed(const Graph<T>& graph) {
    enum class Colours { WHITE = -1, GREY = 0, BLACK = 1 };
    unordered_map<T, Colours> colourMap;
    for (const auto& [node, _] : graph)
        colourMap[node] = Colours::WHITE;

    function<bool(const T&)> dfs_traverse = [&](const T& curr) -> bool {
        colourMap[curr] = Colours::GREY;

        for (const auto& neighbour : graph.at(curr))
            if (colourMap[neighbour] == Colours::GREY || 
                (colourMap[neighbour] == Colours::WHITE && dfs_traverse(neighbour)))
                return true;

        colourMap[curr] = Colours::BLACK;
        return false;
    };

    for (const auto& [node, colour] : colourMap)
        if (colour == Colours::WHITE && dfs_traverse(node))
            return true;

    return false;
}

template <class T>
bool is_cyclic_undirected(const Graph<T>& graph) {
    unordered_set<T> visited;
    for (const auto& [node, _] : graph) {
        if (visited.find(node) != visited.end()) 
            continue;

        queue<pair<T, T>> queue;
        queue.push({node, node});
        visited.insert(node);

        while(!queue.empty()) {
            const auto& [curr, parent] = queue.front();
            queue.pop();

            for (const T& neighbour : graph.at(curr)) {
                if (neighbour == curr) 
                    return true;

                if (visited.find(neighbour) == visited.end()) {
                    queue.push({neighbour, curr});
                    visited.insert(neighbour);
                    continue;
                }

                // neighbour has already been visited; if it isn't the parent of curr => cycle
                if (neighbour != parent)
                    return true;
            }
        }
    }
    
    return false;
}

TEST_CASE("Find cycle in directed graph") {
    Graph<string> graph;

    SUBCASE("Empty graph") {
        graph = {};
        CHECK(is_cyclic_directed(graph) == false);
    }

    SUBCASE("Single cycle") {
        graph = {
            {"A", {"B"}},
            {"B", {"C", "D"}},
            {"C", {"A"}},
            {"D", {}}
        };
        CHECK(is_cyclic_directed(graph) == true);
    }

    SUBCASE("Back-and-forth cycle") {
        graph = {
            {"A", {"B"}},
            {"B", {"A"}}
        };
        CHECK(is_cyclic_directed(graph) == true);
    }

    SUBCASE("No cycle") {
        graph = {
            {"A", {"B"}},
            {"B", {"C"}},
            {"C", {}}
        };
        CHECK(is_cyclic_directed(graph) == false);
    }

    SUBCASE("Node with self-loop") {
        graph = {
            {"A", {"A"}},
            {"B", {"C"}},
            {"C", {}}
        };
        CHECK(is_cyclic_directed(graph) == true);
    }

    SUBCASE("Disjoint components with no cycle") {
        graph = {
            {"A", {"B"}},
            {"B", {"C", "D"}},
            {"C", {}},
            {"D", {}},
            {"E", {"F"}},
            {"F", {}}
        };
        CHECK(is_cyclic_directed(graph) == false);
    }
    
    SUBCASE("Disjoint components with one cycle") {
        graph = {
            {"A", {"B"}},
            {"B", {"C", "D"}},
            {"C", {}},
            {"D", {"A"}},
            {"E", {"F"}},
            {"F", {}}
        };
        CHECK(is_cyclic_directed(graph) == true);
    }

    SUBCASE("Disjoint components with two cycles") {
        graph = {
            {"A", {"B"}},
            {"B", {"C"}},
            {"C", {"A"}},
            {"D", {"E"}},
            {"E", {"F"}},
            {"F", {"D"}}
        };
        CHECK(is_cyclic_directed(graph) == true);
    }

    SUBCASE("Complex graph with cycles") {
        graph = {
            {"A", {"B", "C"}},
            {"B", {"C", "D"}},
            {"C", {"A"}},
            {"D", {}},
            {"E", {"F"}},
            {"F", {"E"}}
        };
        CHECK(is_cyclic_directed(graph) == true);
    }

    SUBCASE("Complex graph without cycles") {
        graph = {
            {"A", {"B", "C"}},
            {"B", {"C", "D"}},
            {"C", {}},
            {"D", {}},
            {"E", {"F"}},
            {"F", {}}
        };
        CHECK(is_cyclic_directed(graph) == false);
    }
}


TEST_CASE("Find cycle in undirected graph") {
    Graph<string> graph;

    SUBCASE("Empty graph") {
        graph = {};
        CHECK(is_cyclic_undirected(graph) == false);
    }

   SUBCASE("Node with self-loop") {
        graph = {
            {"A", {"A"}},
            {"B", {"C"}},
            {"C", {"B"}}
        };
        CHECK(is_cyclic_undirected(graph) == true);
    }

    SUBCASE("Single vertex") {
        graph = {
            {"A", {"B"}},
            {"B", {"A"}}
        };
        CHECK(is_cyclic_undirected(graph) == false);
    }
    
    SUBCASE("Single cycle") {
        graph = {
            {"A", {"B", "C"}},
            {"B", {"A", "C"}},
            {"C", {"A", "B"}}
        };
        CHECK(is_cyclic_undirected(graph) == true);
    }

    SUBCASE("No cycle") {
        graph = {
            {"A", {"B"}},
            {"B", {"A", "C"}},
            {"C", {"B"}}
        };
        CHECK(is_cyclic_undirected(graph) == false);
    }
    
    SUBCASE("Disjoint components with no cycle") {
        graph = {
            {"A", {"B"}},
            {"B", {"A", "C", "D"}},
            {"C", {"B"}},
            {"D", {"B"}},

            {"E", {"F"}},
            {"F", {"E"}}
        };
        CHECK(is_cyclic_undirected(graph) == false);
    }
    
    SUBCASE("Disjoint components with one cycle") {
        graph = {
            {"A", {"B", "D"}},
            {"B", {"A", "C", "D"}},
            {"C", {"B"}},
            {"D", {"B", "A"}},
            
            {"E", {"F"}},
            {"F", {"E"}}
        };
        CHECK(is_cyclic_undirected(graph) == true);
    }

    SUBCASE("Disjoint components with two cycles") {
          graph = {
            {"A", {"B", "D"}},
            {"B", {"A", "D"}},
            {"D", {"B", "A"}},

            {"C", {"E", "F"}},
            {"E", {"F", "C"}},
            {"F", {"E", "C"}}
        };
        CHECK(is_cyclic_undirected(graph) == true);
    }
}


int main() {
    doctest::Context().run();
    return 0;
}