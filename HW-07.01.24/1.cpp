/**
 Да се напише програма, която от стандартния вход чете цяло положително число N, след това прочита N масива. 
 За всеки масив първо се въвежда големината му, а след това се въвеждат елементите.

 Програмата проверява дали всеки един от масивите е сортиран. Ако всеки един от тях е сортиран, програмата създава нов масив, 
 съдържащ елементите на масивите, въведени от потребителя, но в сортиран вид, и го извежда на стандартния изход.
 В противен случай се извежда подходящо съобщение за грешка.
 Забележка: При сортирането всеки масив трябва да бъде обходен точно веднъж.
 (Т.е решения използвайки bubble/selection sort няма да се приемат)

 Вход:
 How many arrays: >3
 >3
 >1 2 3
 >4
 >1 4 5 145
 >5
 >3 6 7 8 9
 Изход:
 1 1 2 3 3 4 5 6 7 8 9 145

 Вход:
 How many arrays: >2
 >1
 >121
 >2
 >1 0
 Изход:
 All input arrays should be sorted.
* */

#include <iostream>

#define ROW_METADATA_SLOTS 2

using namespace std;

// ##  Utilities ##

int getRowSize(int *row) {
    return row[0];
}

int *getRowPointerIndex(int *row) {
    return &row[1];
}

int *getRowPointerValue(int *row) {
    int index = *getRowPointerIndex(row);
    return index != -1 ? &row[index] : nullptr;
}

void freeMatrix(int **matrix, size_t rows) {
    for (int i = 0; i < rows; i++)
        delete[] matrix[i];
    delete[] matrix;
}

void printMtx(int **mtx, size_t rows) {
    cout << endl;
    for (int i = 0; i < rows; i++) {
        int rowSize = getRowSize(mtx[i]);
        for (int j = 0 + ROW_METADATA_SLOTS; j < rowSize + ROW_METADATA_SLOTS; j++)
            cout << mtx[i][j] << ' ';
        cout << endl;
    }
    cout << endl;
}

bool isRowSorted(int *row) {
    size_t size = getRowSize(row);
    if (size == 1)
        return true;

    for (size_t i = 1 + ROW_METADATA_SLOTS; i < size + ROW_METADATA_SLOTS; i++) {
        if (row[i] < row[i - 1])
            return false;
    }
    return true;
}

bool shouldAbort(int **mtx, size_t arrays) {
    for (size_t i = 0; i < arrays; i++) {
        bool sorted = isRowSorted(mtx[i]);
        if (!sorted)
            return true;
    }
    return false;
}

int getMinOf(const int *arr, const size_t &size) {
    int result = arr[0];
    for (int i = 1; i < size; ++i) {
        result = result < arr[i] ? result : arr[i];
    }
    return result;
}

void insertElement(int *array, const int element) {
    int size = getRowSize(array);
    for (size_t i = 1; i < size + 1; i++) {
        if (array[i] != 0) //  int arrays' elements default to 0 -> not 0 => element has been assigned
            continue;

        array[i] = element;
        return;
    }
}


// ##  Actual program ##


/**
 * Each row has the following metadata: \n
 * row[0] is the count of the elements in the row \n
 * row[1] is the current pointer index position \n
 * ... onwards are the row elements \n
*/
int *createRow(const int &size) {
    int *row = new int[size + ROW_METADATA_SLOTS];
    row[0] = size;
    for (size_t i = (0 + ROW_METADATA_SLOTS); i < size + ROW_METADATA_SLOTS; i++) {
        int num;
        cin >> num;
        row[i] = num;
    }
    row[1] = 0 + ROW_METADATA_SLOTS;

    return row;
}

int **createMatrix(size_t rows) {
    int **mtx = new int *[rows];

    for (int i = 0; i < rows; i++) {
        int size;
        cin >> size;

        int *row = createRow(size);
        mtx[i] = row;
    }

    return mtx;
}

int getMinOfRowPointers(int **mtx, const size_t &size) {
    size_t nonExhaustedRows = 0;
    for (int i = 0; i < size; ++i) {
        nonExhaustedRows = *getRowPointerIndex(mtx[i]) != -1 ? nonExhaustedRows + 1 : nonExhaustedRows;
    }

    int *mins = new int[nonExhaustedRows];

    //  Extract only the RowPointerValues whose RowPointerIndex != -1
    size_t counter = 0, index = 0;
    while (counter < nonExhaustedRows) {
        if (*getRowPointerIndex(mtx[index]) == -1) {
            index++;
            continue;
        }

        int value = *getRowPointerValue(mtx[index]);
        mins[counter] = value;

        index++;
        counter++;
    }

    int min = getMinOf(mins, nonExhaustedRows);
    delete[] mins;
    return min;
}

void moveRowPointer(int *row) {
    int size = getRowSize(row);
    int *position = getRowPointerIndex(row);
    *position = *position + 1 == size + ROW_METADATA_SLOTS ? -1 : (*position) + 1;
}

void iterateRowPointersOnce(int **mtx, const size_t &size, int *result) {
    for (size_t i = 0; i < size; ++i) {
        int *current = getRowPointerValue(mtx[i]);
        if (current == nullptr)
            continue;
        int min = getMinOfRowPointers(mtx, size);
        if (min == *current) {
            insertElement(result, *current);
            moveRowPointer(mtx[i]);
        }
    }
}

int *createSortedConcat(int **mtx, const size_t &size) {
    int results = 0;
    for (size_t i = 0; i < size; i++) {
        results += getRowSize(mtx[i]);
    }

    int *result = new int[results + 1];
    result[0] = results;

    for (int j = 0; j < results; ++j) {
        iterateRowPointersOnce(mtx, size, result);
    }

    return result;
}

int main() {
    size_t arrays;
    cout << "How many arrays?: ";
    cin >> arrays;

    int **mtx = createMatrix(arrays);

    printMtx(mtx, arrays);

    if (shouldAbort(mtx, arrays)) {
        cout << "All input arrays should be sorted" << endl;
        freeMatrix(mtx, arrays);
        return 0;
    }
    cout << "==========" << endl;

    int *result = createSortedConcat(mtx, arrays);
    int size = getRowSize(result);
    for (size_t i = 1; i < size + 1; i++) {
        cout << result[i] << " ";
    }
    cout << endl;

    freeMatrix(mtx, arrays);
    delete result;

    return 0;
}