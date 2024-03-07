#include <iostream>

using namespace std;

// Function to swap two elements in an array
void swap(int &a, int &b)
{
    int temp = a;
    a = b;
    b = temp;
}

// Function to check if an array contains a specific element
bool contains(const int arr[], const int &size, const int &value)
{
    for (int i = 0; i < size; ++i)
        if (arr[i] == value)
            return true;
    return false;
}

void copyArrayTo(const int *source, const int &sourceSize, int *&destination, int &destinationSize)
{
    destination = new int[destinationSize];
    for (int i = 0; i < sourceSize; ++i)
    {
        destination[i] = source[i];
    }
}

void addElementToArray(int *&source, unsigned &size, const int &newElement)
{
    int *updatedResult;
    int updatedSize = size + 1;
    copyArrayTo(source, size, updatedResult, updatedSize);

    updatedResult[size++] = newElement;

    delete[] source;        // Free the memory of the old array
    source = updatedResult; // Point to the updated result array
}

// Function to generate all unique permutations of digits
void generatePermutations(unsigned short digits[], int start, int end, int *&results, unsigned &currentIndex)
{
    if (start == end)
    {
        int newNumber = digits[0] * 1000 + digits[1] * 100 + digits[2] * 10 + digits[3];

        if (newNumber < 1000 || contains(results, currentIndex, newNumber))
            return;

        addElementToArray(results, currentIndex, newNumber);

        return;
    }

    for (int i = start; i <= end; ++i)
    {
        // Swap the current element with each element after it
        swap(digits[start], digits[i]);
        // Recursively generate permutations for the remaining elements
        generatePermutations(digits, start + 1, end, results, currentIndex);
        // Swap back to restore the original order for the next iteration
        swap(digits[start], digits[i]);
    }
}

int main()
{
    unsigned short num;
    while (num < 1000 || num > 9999)
    {
        cout << "Enter a 4-digit integer: ";
        cin >> num;
    }

    // Extract digits from the input number
    const unsigned short numDigits = 4;
    unsigned short digits[numDigits];
    for (int i = numDigits - 1; i >= 0; --i)
    {
        digits[i] = num % 10;
        num /= 10;
    }

    int *results = new int[0]; // Initially, an empty array
    unsigned currentIndex = 0;
    generatePermutations(digits, 0, numDigits - 1, results, currentIndex);

    // Print the results array
    cout << "Permutations: ";
    for (int i = 0; i < currentIndex; ++i)
    {
        cout << results[i] << " ";
    }
    cout << endl;

    delete[] results;

    return 0;
}
