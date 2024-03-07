#include <iostream>

using namespace std;

void printLayer(const int &layer, const int &width)
{
    const int skip = width - layer;
    for (size_t i = 0; i < skip; i++)   //  skip characters for layer indent
        cout << " ";    
    
    for (size_t i = 0; i < layer; i++)  //  print the leftside leaves
        cout << "*";    

    cout << " | ";                      //  print the thrunk

    for (size_t i = 0; i < layer; i++)  //  print the rightside leaves
        cout << "*";

    cout << endl;
}

void printXMasTree(const int &size)
{
    int layer = 0;
    while (layer != size + 1) {
        printLayer(layer, size);
        layer++;
    }
}

int main()
{
    int size;
    while (size < 1 || size > 100)
    {
        cout << "Enter N between 1 and 100: ";
        cin >> size;
    }

    printXMasTree(size);

    return 0;
}