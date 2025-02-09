#include <algorithm>
#include <fstream>
#include <iostream>
#include <vector>

using namespace std;

template <typename T>
ostream& operator<<(ostream& os, const vector<T>& vec) {
    os << "[ ";
    for (const auto& item : vec) {
        os << item << " ";
    }
    os << ']';
    return os;
}

void countingSort(std::vector<int>& arr) {
    if (arr.empty()) return;

    int maxVal = *max_element(arr.begin(), arr.end());
    int minVal = *min_element(arr.begin(), arr.end());
    int range = maxVal - minVal + 1;

    vector<int> counts(range, 0);

    for (int num : arr) {
        ++counts[num - minVal];
    }

    for (auto it = ++counts.begin(); it < counts.end(); ++it) {
        *it += *(it - 1);
    }

    vector<int> res(arr.size());
    for(auto it = arr.rbegin(); it != arr.rend(); ++it) {
        int num = *it;
        res[counts[num-minVal]] = num;
        counts[num-minVal]--;
    }

    arr = move(res);
}

int main() {
    std::vector<int> arr = {4, 2, 2, 8, 3, 3, 1, 5, 7, 6, 7, 9, 8};

    cout << arr << endl;
    countingSort(arr);
    cout << arr << endl;

    return 0;
}
