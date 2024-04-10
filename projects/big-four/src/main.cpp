#include <iostream>
#include <public/DynArray.h>

int main() {
    int a[] = {1 ,2, 3, 4, 5, 6};
    size_t size = sizeof(a) / sizeof(a[0]);

    DynArray<int> arr(a, size);
    std::cout << arr << std::endl;

    return 0;
}