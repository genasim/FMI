#include <iostream>

#include "DfsTraversalsCollection.h"

using namespace std;

int main() {
    Graph graph;
    graph[0] = {1, 2};
    graph[1] = {2, 3};
    graph[2] = {1};
    graph[3] = {0};

    DfsTraversalsCollection traversals(graph, 0);
    for (const DfsTraversal& traversal : traversals) {
        std::cout << traversal << std::endl;
    }

    return 0;
}
