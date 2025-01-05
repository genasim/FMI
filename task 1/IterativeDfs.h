#pragma once
#include "BaseDfsTraversal.h"
#include <stack>
#include <algorithm>

class IterativeDfs : public BaseDfsTraversal {
public:
    using BaseDfsTraversal::BaseDfsTraversal;

    void runDfs(int start) override {
        if (!inRange(start)) return;
        if (!isWhite(start)) return;

        std::stack<int> st;
        st.push(start);

        while (!st.empty()) {
            int u = st.top();
            st.pop();

            if (color[u] == WHITE) {
                color[u] = GRAY;
                disc[u] = ++time;
                order.push_back(u);
            }

            const auto& neighbors = graph[u];
            for (int i = neighbors.size() - 1; i >= 0; i--) {
                int v = neighbors[i];
                if (color[v] == WHITE) {
                    
                    treeEdges.emplace_back(u, v);
                    st.push(v);
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
    }
};
