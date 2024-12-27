#include <iostream>
#include <vector>
#include <queue>

template <typename T>
std::ostream& operator<<(std::ostream& os, const std::vector<T>& vec) {
    os << "[ ";
    for (const auto& elem : vec) {
        os << elem << " ";
    }
    os << "]";
    return os;
}

std::vector<int> mergeSortedVectors(const std::vector<std::vector<int>>& vectors) {
    struct Node {
    private:
        using It = std::vector<int>::const_iterator;
    public:
        It current, end;
        Node(It curr, It end) : current(curr), end(end) {}
    };
    
    struct NodeCompare {
        bool operator()(const Node& a, const Node& b) const {
            return *(a.current) > *(b.current);
        }
    };
    
    std::priority_queue<Node, std::vector<Node>, NodeCompare> minHeap;
    for (const auto& vector : vectors) {
        if (!vector.empty())
            minHeap.emplace(vector.cbegin(), vector.cend());
    }
    
    std::vector<int> result;
    while (!minHeap.empty()) {
        Node minNode = minHeap.top();
        minHeap.pop();
        
        result.push_back(*minNode.current);
        
        auto nexIter = ++minNode.current;
        if (nexIter != minNode.end)
            minHeap.emplace(nexIter, minNode.end);
    }
    
    return result;
}

int main() {
    std::vector<std::vector<int>> vectors = {{9, 10, 12}, {14, 17, 18}, {}, {1, 10, 11, 12, 22, 90}, {13}, {16, 18}};
    
    std::cout << vectors << std::endl;
    std::cout << mergeSortedVectors(vectors) << std::endl;
    
    return 0;
}
