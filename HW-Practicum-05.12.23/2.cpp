#include <iostream>

using namespace std;

bool canCross(int arr[], const int &size) {
    int sum = 0;
    for (size_t i = 0; i < size; i++)
        sum += arr[i];

    return sum >= size;    
}

int main()
{
    int arr[] = {1,0,0,0};
    const int size = sizeof(arr) / sizeof(arr[0]);

    bool result = canCross(arr, size);

    cout << (result ? "Yes" : "No") << endl;

    return 0;
}