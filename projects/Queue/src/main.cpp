#include <iostream>
#include <vector>

#include "Queue.h"
#include "Vector.h"

int main() {
    Vector<int> vector({-1, -2, -3, -4, -5, -6, -7, -8, -9});
    std::cout << vector << std::endl;
    vector.erase(vector[3]);
    std::cout << vector << std::endl;

    Queue<int> queue({1, 2, 3, 4});

    std::cout << queue.size() << std::endl;
    queue.push(-1);
    queue.pop();
    queue.push(1000);
    queue.pop();

    std::cout << queue << std::endl;

    return 0;
}
