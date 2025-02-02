#include <fstream>
#include <iostream>
#include <map>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <queue>

using namespace std;

template <class Vertex, class Weight>
using WGraph = unordered_map<Vertex, map<Vertex, Weight>>;

vector<string> all_words_between_nodes_simple_path(
    const WGraph<int, string>& graph, int start, int dest) {
    unordered_set<int> visited;
    vector<string> words;

    auto helper = [&graph, dest](auto&& self, string word, int start,
                                 unordered_set<int>& visited,
                                 vector<string>& allWords) {
        if (visited.find(start) != visited.end()) {
            return;
        }

        if (start == dest) {
            allWords.push_back(word);
            return;
        }

        visited.insert(start);
        for (auto [vertex, letters] : graph.at(start)) {
            for (char letter : letters) {
                self(self, word + letter, vertex, visited, allWords);
            }
        }
        visited.erase(start);
    };

    helper(helper, "", 2, visited, words);

    return words;
}

vector<string> all_words_between_nodes_max_lenght(
    const WGraph<int, string>& graph, int start, int dest, size_t max_length) {
    vector<string> words;

    auto helper = [&graph, dest, max_length](auto&& self, string word,
                                             int start,
                                             vector<string>& allWords) {
        if (start == dest || word.size() == max_length) {
            allWords.push_back(word);
            return;
        }

        for (auto [vertex, letters] : graph.at(start)) {
            for (char letter : letters) {
                self(self, word + letter, vertex, allWords);
            }
        }
    };

    helper(helper, "", 2, words);

    return words;
}

template <class T>
ostream& operator<<(ostream& os, const vector<T>& vector) {
    os << "[ ";
    for (auto&& item : vector) {
        os << item << ' ';
    }
    os << ']';
    return os;
}

int main() {
    WGraph<int, string> graph;
    graph[2][3] = "ab";
    graph[3][2] = 'e';
    graph[3][4] = 'c';

    auto words_unique = all_words_between_nodes_simple_path(graph, 2, 4);
    cout << words_unique << endl;

    auto words_length = all_words_between_nodes_max_lenght(graph, 2, 4, 5);
    cout << words_length << endl;

    return 0;
}