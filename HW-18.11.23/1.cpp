#include <iostream>

using namespace std;

int pow(int base, int power)
{
    int result = 1;
    for (short i = 1; i <= power; i++)
    {
        result *= base;
    }
    return result;
}

bool areValid(const unsigned &a, const unsigned &b)
{
    if (a < b)
    {
        return false;
    }

    unsigned tempA = a;
    unsigned short numDigitsA = 0;
    while (tempA > 0)
    {
        numDigitsA++;
        tempA /= 10;
    }

    unsigned tempB = b;
    unsigned short numDigitsB = 0;
    while (tempB > 0)
    {
        numDigitsB++;
        tempB /= 10;
    }

    if (numDigitsA - numDigitsB != 1)
    {
        return false;
    }

    return true;
}

void problemA()
{
    unsigned a = 0, b = 0;
    cin >> a >> b;

    if (!areValid(a, b))
    {
        cout << "No" << endl;
        return;
    }

    unsigned tempA = a;
    unsigned short numDigitsA = 0;
    while (tempA > 0)
    {
        numDigitsA++;
        tempA /= 10;
    }

    for (unsigned i = 0; i < numDigitsA; ++i)
    {
        unsigned divisor = pow(10, i + 1);
        unsigned modified_a = a / divisor * (divisor / 10) + a % pow(10, i);

        if (modified_a == b)
        {
            cout << "Yes" << endl;
            return;
        }
    }

    cout << "No" << endl;
}

bool contains(int A[], int sizeA,int B[], int sizeB)
{
    int i = 0, j = 0;
    while (i < sizeA && j < sizeB)
    {
        if (A[i] == B[j])
        {
            ++i;
            ++j;
        }
        else
        {
            ++i;
        }
    }

    return j == sizeB;
}

void problemB()
{
    int A[] = {1, 2, 4, 2, 3};
    int B[] = {2, 2, 4};

    int sizeA = sizeof(A) / sizeof(A[0]);
    int sizeB = sizeof(B) / sizeof(B[0]);

    cout << (contains(A, sizeA, B, sizeB) ? "true" : "false") << endl; 
}

int main()
{
    // problemA();
    problemB();

    return 0;
}