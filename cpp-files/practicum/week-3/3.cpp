#include <iostream>
#include <vector>
#include <cctype>
#include <cassert>

#define ALPHABET_SIZE 26

template <typename T>
std::ostream& operator<<(std::ostream& os, const std::vector<T>& vec) {
    os << "[ ";
    for (const auto& elem : vec) {
        os << elem << " ";
    }
    os << "]";
    return os;
}

void sortWordsByFirstLetter(std::vector<std::string>& words) {
    std::vector<std::vector<std::string>> letterBuckets(ALPHABET_SIZE);

    for (const std::string& word : words) {
        char firstChar = std::tolower(word[0]);
        char letter = firstChar - 'a';

        assert(letter >= 0 && letter < ALPHABET_SIZE);
        letterBuckets[letter].push_back(word);
    }

    int position = 0;
    for (const auto& bucket : letterBuckets) {
        for (const std::string& word : bucket) {
            words[position++] = word;
        }
    }
}

int main() {
    std::vector<std::string> words = {"banana", "apple", "Alpaca", "Cat", "biscuit", "bat", "elephant", "string", "house", "progress", "Trousers", "mouse"};

    sortWordsByFirstLetter(words);

    std::cout << "Sorted words: " << words << std::endl;

    return 0;
}
