#include <iostream>

using namespace std;

int getNum(const int &n)
{
    if (n == 1)
        return 1;
    if (n == 2)
        return 2;
    if (n == 3)
        return 3;

    return getNum(n - 1) * getNum(n - 1 - 2);
}

int main()
{
    int indecies[] = {1, 2, 3, 4, 5, 6, 7, 8};
    for (const auto &num : indecies)
        cout << getNum(num) << " ";
    cout << endl;

    return 0;
}