#include <iostream>

using namespace std;

int compare(const int a, const int b) {
    if(a > b)
        return 1;
    
    if(a < b)
        return -1;

    return 0;
}

int arrDifference(const int a[], const int b[], const size_t size, int result = 0) {
    if (size == 0)
        return result;

    int diff = compare(a[size - 1], b[size -1]);
    return  arrDifference(a, b, size - 1, result + diff);
}

int main() {
    int a[] = {1, 4, 6, 3, 5};
    int b[] = {5, 4, 2, 1, 3};
    size_t size = sizeof(a) / sizeof(a[0]);

    int diff = arrDifference(a, b, size);
    cout << diff << endl;   // 2

    return 0;
}