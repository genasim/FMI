#pragma once
#include "RecursiveDfsTraversal.h"

class DefaultDfs : public RecursiveDfsTraversal {
public:
    using RecursiveDfsTraversal::RecursiveDfsTraversal;

protected:
    std::vector<int> getNeighbors(int u) override {
        return graph[u];
    }
};
