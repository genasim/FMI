#pragma once
#include "RecursiveDfsTraversal.h"
#include <algorithm>

class ReverseDfs : public RecursiveDfsTraversal {
public:
    using RecursiveDfsTraversal::RecursiveDfsTraversal;

protected:
    std::vector<int> getNeighbors(int u) override {
        std::vector<int> result = graph[u];
        std::reverse(result.begin(), result.end());
        return result;
    }
};
