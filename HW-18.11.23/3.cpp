#include <iostream>
#include <iomanip>

using namespace std;

// Function to check if a number has distinct digits
bool hasDistinctDigits(int num, int M) {
    bool digits[10] = {false};  // Array to check for repeated digits
    int count = 0;

    while (num > 0) {
        int digit = num % 10;
        if (digits[digit] || digit == 0 || digit >= 10 || digit < 0) {
            return false;
        }
        digits[digit] = true;
        num /= 10;
        count++;
    }

    return count == M;
}

// Function to check if a guess matches the specified number of bulls and cows
bool matchesGuess(int guess, int target, int bulls, int cows) {
    int guessDigits[10] = {0};
    int targetDigits[10] = {0};

    // Count bulls and cows
    int countBulls = 0, countCows = 0;

    while (guess > 0 && target > 0) {
        int guessDigit = guess % 10;
        int targetDigit = target % 10;

        if (guessDigit == targetDigit) {
            countBulls++;
        } else {
            guessDigits[guessDigit]++;
            targetDigits[targetDigit]++;
        }

        guess /= 10;
        target /= 10;
    }

    for (int i = 0; i < 10; i++) {
        countCows += min(guessDigits[i], targetDigits[i]);
    }

    return countBulls == bulls && countCows == cows;
}

int main() {
    int M, N;
    cin >> M >> N;

    int* guesses = new int[N];
    int* bulls = new int[N];
    int* cows = new int[N];

    for (int i = 0; i < N; i++) {
        cin >> guesses[i] >> bulls[i] >> cows[i];
    }

    for (int num = 0; num <= 999999; num++) {
        if (hasDistinctDigits(num, M)) {
            bool valid = true;

            for (int i = 0; i < N; i++) {
                if (!matchesGuess(num, guesses[i], bulls[i], cows[i])) {
                    valid = false;
                    break;
                }
            }

            if (valid) {
                cout << setw(M) << setfill('0') << num << " ";
            }
        }
    }

    delete[] guesses;
    delete[] bulls;
    delete[] cows;

    return 0;
}
