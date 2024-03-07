#include <iostream>
#include <fstream>

using namespace std;

struct Results {
  Results(const int a, const int b, const int c) {
      sum = (a + b + c);
      mult = (a * b * c);
  };

  int sum;
  int mult;  
};

void writeToFile(const char* const &path, const Results &input) {
    ofstream file(path);

    if (!file.is_open()) {
        throw runtime_error("Couldnt open file");
    }

    file << input.sum << " | " << input.mult;

    file.close();
}

int main() {
    int a, b, c;
    cin >> a >> b >> c;

    Results pair(a,b,c);

    writeToFile("../files-sem.02/tmp/result.txt", pair); // pwd is the bin/ dir
    cout << (pair.sum - pair.mult) << endl;

    return 0;
}