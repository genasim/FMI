#include <iostream>
#include <vector>
#include <climits>

template <typename T>
std::ostream& operator<<(std::ostream& os, const std::vector<T>& vec) {
    os << "[ ";
    for (const auto& elem : vec) {
        os << elem << " ";
    }
    os << "]";
    return os;
}

template <typename T>
using ConstVecIterators = std::vector<typename std::vector<T>::const_iterator>;

std::vector<int> mergeSortedVectors(const std::vector<std::vector<int>>& vectors) {
    if (vectors.empty()) return {};

    std::vector<int> result;
    ConstVecIterators<int> iterators;
    for (auto &&vector : vectors)
        iterators.push_back(vector.cbegin());
    
    while (true) {
        int minValue = INT_MIN;
        int minIndex = -1;

        for (size_t i = 0; i < vectors.size(); ++i) {
            if (iterators[i] == vectors[i].cend()) continue;
            
            if (*iterators[i] < minValue) {
                minValue = *iterators[i];
                minIndex = i;
            }
        
        }

        if (minIndex == -1) {
            break;
        }

        result.push_back(minValue);
        iterators[minIndex]++;
    }
    return result;
}

int main() {
    std::vector<std::vector<int>> vectors = {{9, 10, 12}, {14, 17, 18}, {10, 11, 12, 22, 90}, {13}, {16, 18}};

    std::cout << vectors << std::endl;
    std::cout << mergeSortedVectors(vectors) << std::endl;

    return 0;
}
