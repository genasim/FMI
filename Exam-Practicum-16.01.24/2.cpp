#include <iostream>

using namespace std;

int matchColumn(int* arr, const unsigned& colSize, int* pattern, const unsigned& patSize) {
    int
}

int getIndexOfHighest(const int arr[], const unsigned &size) {
    int index = 0;
    for (int i = 0; i < size-1; ++i) {
        if (arr[i] < arr[i+1])
            index = i;
    }
    return index;
}

int findLongestSubColumn(int mtx[][5], const unsigned &rows, int arr[], unsigned const &size ) {
    int results[] = {0, 0, 0, 0, 0}; // keep track of matches

    for (int i = 0; i < 5; ++i) {
        int column[rows];
        for (int j = 0; j < rows; ++j)
            column[j] = mtx[j][i];
        results[i] = matchColumn(column, rows, );
    }

    int result = getIndexOfHighest(results, 5);
    return result;
}

int main() {
    int mtx[][5] = {
            {4, 4, 4, 7, 4},
            {3, 3, 2, 7, 3},
            {0, 2, 2, 9, 2},
            {0, 7, 1, 6, 8},
    };
    unsigned rows = sizeof(mtx) / sizeof(int[5]);
    int arr[] = {4, 3, 2, 1};
    unsigned size = sizeof(arr) / sizeof(int);

    int column = findLongestSubColumn(mtx, rows, arr, size);

    return 0;
}