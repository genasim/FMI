#pragma once
#include "BaseDfsTraversal.h"
#include <vector>


class RecursiveDfsTraversal : public BaseDfsTraversal {
public:
    using BaseDfsTraversal::BaseDfsTraversal; 
    
    void runDfs(int start) override {
        if (!inRange(start)) return;
        if (isWhite(start)) {
            dfs(start);
        }
    }

protected:
    virtual std::vector<int> getNeighbors(int u) = 0;

    void dfs(int u) {
        color[u] = GRAY;
        disc[u] = ++time;
        order.push_back(u);

        std::vector<int> neigh = getNeighbors(u);

        for (int v : neigh) {
            if (isWhite(v)) {
                
                treeEdges.emplace_back(u, v);
                dfs(v);
            }
            else if (color[v] == GRAY) {
                
                backEdges.emplace_back(u, v);
            }
            else {
                
                if (disc[u] < disc[v]) {
                    forwardEdges.emplace_back(u, v);
                } else {
                    crossEdges.emplace_back(u, v);
                }
            }
        }

        color[u] = BLACK;
    }
};
