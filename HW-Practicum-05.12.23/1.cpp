#include <iostream>

using namespace std;

int findLongestAPStartIndex(int arr[], int size) {
    if (size <= 2) {
        // An arithmetic progression must have at least three elements
        return -1; // Return -1 to indicate no valid arithmetic progression
    }

    int maxLength = 2; // Initial length of the longest arithmetic progression
    int startIndex = 0; // Initial starting index of the longest arithmetic progression

    for (int i = 0; i < size - 1; ++i) {
        for (int j = i + 1; j < size; ++j) {
            int diff = arr[j] - arr[i];
            int currentLength = 2; // Current length of the arithmetic progression

            int currentNum = arr[j];
            int currentIndex = j;

            while (true) {
                int nextNum = currentNum + diff;
                int nextIndex = -1;

                for (int k = currentIndex + 1; k < size; ++k) {
                    if (arr[k] == nextNum) {
                        nextIndex = k;
                        break;
                    }
                }

                if (nextIndex != -1) {
                    // Found the next element in the arithmetic progression
                    currentNum = arr[nextIndex];
                    currentIndex = nextIndex;
                    currentLength++;
                } else {
                    // Next element not found, break the loop
                    break;
                }
            }

            // Update if the current progression is longer
            if (currentLength > maxLength) {
                maxLength = currentLength;
                startIndex = i;
            }
        }
    }

    return startIndex;
}

int main()
{
    int source[] = {0, 2, 4, 5, 2, 3, -1, -5, -9, -6, -3, 5};
    const int size = sizeof(source) / sizeof(int);

    int index = findLongestAPStartIndex(source, size);
    
    cout << index << endl;

    return 0;
}