#pragma once
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <memory>
#include "DefaultDfs.h"
#include "ReverseDfs.h"
#include "LexDfs.h"
#include "IterativeDfs.h"

struct TraversalResult {
    std::string name;
    std::vector<int> order;
    BaseDfsTraversal* dfsPtr;
    
    bool operator<(const TraversalResult& other) const {
        return order < other.order;
    }
};

class DfsTraversalsCollection {
private:
    const std::vector<std::vector<int>>& graph;
    int n;
    int start;

    BaseDfsTraversal* dfsDefault;
    BaseDfsTraversal* dfsReverse;
    BaseDfsTraversal* dfsLex;
    BaseDfsTraversal* dfsIter;

    std::vector<TraversalResult> allTraversals;

public:
    DfsTraversalsCollection(const std::vector<std::vector<int>>& g, int startVertex)
        : graph(g)
        , n(g.size())
        , start(startVertex)
        , dfsDefault(nullptr)
        , dfsReverse(nullptr)
        , dfsLex(nullptr)
        , dfsIter(nullptr)
    {
        dfsDefault = new DefaultDfs(graph);
        dfsReverse = new ReverseDfs(graph);
        dfsLex     = new LexDfs(graph);
        dfsIter    = new IterativeDfs(graph);

        for (int v = 0; v < n; v++) {
            dfsDefault->runDfs(v);
            dfsReverse->runDfs(v);
            dfsLex->runDfs(v);
            dfsIter->runDfs(v);
        }

        allTraversals.push_back({"Default",   dfsDefault->getOrder(), dfsDefault});
        allTraversals.push_back({"Reverse",   dfsReverse->getOrder(), dfsReverse});
        allTraversals.push_back({"Lex",       dfsLex->getOrder(),     dfsLex});
        allTraversals.push_back({"Iterative", dfsIter->getOrder(),    dfsIter});

        std::sort(allTraversals.begin(), allTraversals.end());
    }

    ~DfsTraversalsCollection() {
        delete dfsDefault;
        delete dfsReverse;
        delete dfsLex;
        delete dfsIter;
    }

    const std::vector<int>& getDefaultOrder() const { return dfsDefault->getOrder(); }

    std::vector<std::pair<int,int>> getDefaultTreeEdges() const {
        return dfsDefault->getTreeEdges();
    }

    friend std::ostream& operator<<(std::ostream& out, const DfsTraversalsCollection& coll) {
        auto &ord = coll.dfsDefault->getOrder();
        for (size_t i = 0; i < ord.size(); i++) {
            out << ord[i] << (i + 1 < ord.size() ? " " : "");
        }
        return out;
    }

    using const_iterator = std::vector<TraversalResult>::const_iterator;

    const_iterator begin() const {
        return allTraversals.begin();
    }
    const_iterator end() const {
        return allTraversals.end();
    }
};
