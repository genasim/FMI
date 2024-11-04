#include <algorithm>
#include <iostream>
#include <vector>

template <typename T>
std::ostream& operator<<(std::ostream& os, const std::vector<T>& vec) {
    os << "[ ";
    for (const auto& elem : vec) {
        os << elem << " ";
    }
    os << "]";
    return os;
}

uint32_t coundDigits(int num) {
    uint32_t count = 0;
    while (num) {
        num /= 10;
        count++;
    }
    return count;
}

void partitionNumbers(std::vector<int>& nums) {
    std::partition(nums.begin(), nums.end(), [](int num) {
        uint32_t digitsCount = coundDigits(num);
        return digitsCount % 2 == 0;
    });
}

int main() {
    std::vector<int> vector = {10, 2, 12, 12, 4, 100, 102020};

    std::cout << vector << std::endl;
    partitionNumbers(vector);
    std::cout << vector << std::endl;

    return 0;
}
