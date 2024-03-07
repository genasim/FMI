#include <iostream>

using namespace std;

void printMtx(int mtx[3][3]) {
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++)
            cout << mtx[i][j] << " ";
        cout << endl;
    }
}

void problem1(int mtx[3][3]) {
    for (int i = 2; i > -1; i--) {
        for (int j = 0; j < 3; j++)
            cout << mtx[i][j] << " ";
        cout << endl; 
    }
    cout << endl;
}

void problem2(int mtx[3][3]) {
    for (int i = 0; i < 3; i++) {
        for (int j = 2; j > -1; j--)
            cout << mtx[i][j] << " ";
        cout << endl; 
    }
    cout << endl;
}

void problem3(int mtx[3][3]) {
    for (int i = 2; i > -1; i--) {
        for (int j = 2; j > -1; j--)
            cout << mtx[i][j] << " ";
        cout << endl; 
    }
    cout << endl;
}

void problem4(int mtx[3][3]) {
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++)
            cout << mtx[j][i] << " ";
        cout << endl; 
    }
    cout << endl;
}

void problem5(int mtx[3][3]) {
    for (int i = 2; i > -1; i--) {
        for (int j = 0; j < 3; j++)
            cout << mtx[j][i] << " ";
        cout << endl; 
    }
    cout << endl;
}

void problem6(int mtx[3][3]) {
    for (int i = 0; i < 3; i++) {
        for (int j = 2; j > -1; j--)
            cout << mtx[j][i] << " ";
        cout << endl; 
    }
    cout << endl;
}

int main() {
    int mtx[3][3] = {
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9},
    };

    printMtx(mtx);
    cout << endl << "====================" << endl << endl;

    problem1(mtx);
    problem2(mtx);
    problem3(mtx);
    problem4(mtx);
    problem5(mtx);
    problem6(mtx);

    return 0;
}