#include <iostream>

using namespace std;

void getSumOfGreater(float arr[], unsigned curr, const unsigned &length, float &result) {
    if (curr + 1 == length)
        return;

    if (arr[curr] >= 0 && arr[curr] > arr[curr + 1])
        result += arr[curr];

    getSumOfGreater(arr, curr + 1, length, result);
}

int main() {
    float arr[] = {2, -1.9, 1.1, 3.5, 1, 0, 8.3};
    unsigned size = sizeof(arr) / sizeof(float);

    float result = 0;
    getSumOfGreater(arr, 0, size, result);

    cout << result << endl;

    return 0;
}