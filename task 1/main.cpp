#include <iostream>
#include "DfsTraversalsCollection.h"

int main() {
    std::vector<std::vector<int>> graph = {
        {1, 2},      // 0 -> {1, 2}
        {3, 5},      // 1 -> {3, 5}
        {1, 4},      // 2 -> {1, 4}
        {5},         // 3 -> {5}
        {3},         // 4 -> {3}
        {1}          // 5 -> {1}
    };

    int startVertex = 0;

    DfsTraversalsCollection dfsColl(graph, startVertex);

    std::cout << "[Operator<<] Default DFS order: " << dfsColl << "\n\n";

    std::cout << "All Traversals in lexicographic order of their DFS sequences:\n\n";
    for (const auto& t : dfsColl) {

        std::cout << "Traversal name: " << t.name << "\n";
        
        std::cout << "   Order: ";
        for (int v : t.order) {
            std::cout << v << " ";
        }
        std::cout << "\n";

        auto* dfsObj = t.dfsPtr;

        auto treeE = dfsObj->getTreeEdges();
        if (!treeE.empty()) {
            std::cout << "   Tree edges: ";
            for (auto &e : treeE) {
                std::cout << "(" << e.first << "->" << e.second << ") ";
            }
            std::cout << "\n";
        }

        auto backE = dfsObj->getBackEdges();
        if (!backE.empty()) {
            std::cout << "   Back edges: ";
            for (auto &e : backE) {
                std::cout << "(" << e.first << "->" << e.second << ") ";
            }
            std::cout << "\n";
        }

        auto fwdE = dfsObj->getForwardEdges();
        if (!fwdE.empty()) {
            std::cout << "   Forward edges: ";
            for (auto &e : fwdE) {
                std::cout << "(" << e.first << "->" << e.second << ") ";
            }
            std::cout << "\n";
        }

        auto crossE = dfsObj->getCrossEdges();
        if (!crossE.empty()) {
            std::cout << "   Cross edges: ";
            for (auto &e : crossE) {
                std::cout << "(" << e.first << "->" << e.second << ") ";
            }
            std::cout << "\n";
        }

        std::cout << "\n";
    }

    return 0;
}
