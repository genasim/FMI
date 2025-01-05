#pragma once
#include <vector>
#include <utility>


enum Color {
    WHITE,
    GRAY, 
    BLACK  
};

class BaseDfsTraversal {
protected:
   
    const std::vector<std::vector<int>>& graph;
    int n;
  
    std::vector<Color> color;
    std::vector<int>   disc; 

    int time;

    std::vector<int> order;

    std::vector<std::pair<int,int>> treeEdges;
    std::vector<std::pair<int,int>> backEdges;
    std::vector<std::pair<int,int>> forwardEdges;
    std::vector<std::pair<int,int>> crossEdges;

public:
   
    BaseDfsTraversal(const std::vector<std::vector<int>>& g)
        : graph(g)
        , n(g.size())
        , color(n, WHITE)
        , disc(n, 0)
        , time(0)
    {}

    virtual ~BaseDfsTraversal() {} 

    virtual void runDfs(int start) = 0;

    const std::vector<int>& getOrder() const {
        return order;
    }

    const std::vector<std::pair<int,int>>& getTreeEdges() const {
        return treeEdges;
    }
    const std::vector<std::pair<int,int>>& getBackEdges() const {
        return backEdges;
    }
    const std::vector<std::pair<int,int>>& getForwardEdges() const {
        return forwardEdges;
    }
    const std::vector<std::pair<int,int>>& getCrossEdges() const {
        return crossEdges;
    }

protected:

    bool isWhite(int v) const {
        return color[v] == WHITE;
    }
    bool inRange(int v) const {
        return (0 <= v && v < n);
    }

};
