#include <fstream>
#include <iostream>
#include <optional>
#include <queue>
#include <stack>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using namespace std;

template <typename T>
ostream& operator<<(ostream& os, const vector<T>& vec) {
    os << "[ ";
    for (const auto& item : vec) {
        os << item << " ";
    }
    os << ']';
    return os;
}

template <class T>
using Graph = unordered_map<T, unordered_set<T>>;

template <class T>
vector<T> shortest_path_bfs(const Graph<T>& graph, const T& start,
                            const T& end) {
    if (start == end) return {};

    queue<T> queue;
    unordered_map<T, optional<T>> visited;

    queue.push(start);
    visited[start] = nullopt;

    bool isFound = false;
    while (!queue.empty() && !isFound) {
        auto curr = queue.front();
        queue.pop();

        for (const T& neighbour : graph.at(curr)) {
            if (neighbour == end) {
                visited[end] = curr;
                isFound = true;
                break;
            }

            if (visited.find(neighbour) == visited.end()) {
                queue.push(neighbour);
                visited[neighbour] = curr;
            }
        }
    }

    if (!isFound) return {};

    vector<T> path = {end};
    optional<T> parent = visited[end];
    while (parent.has_value()) {
        path.insert(path.cbegin(), parent.value());
        parent = visited[parent.value()];
    }
    return path;
}

template <class T, class Weight>
using WeightedGraph = unordered_map<T, unordered_map<T, Weight>>;

template <class T, class Weight>
vector<T> shortest_path_dijkstra(const WeightedGraph<T, Weight>& graph,
                                 const T& start, const T& end) {
    if (start == end) return {start};

    struct NodeMetadata {
        optional<Weight> distance;
        optional<T> parent;
    };

    unordered_map<T, NodeMetadata> visited;
    priority_queue<pair<Weight, T>, vector<pair<Weight, T>>, greater<>> queue;

    queue.emplace(Weight{}, start);
    visited[start] = {Weight{}, nullopt};

    while (!queue.empty()) {
        auto [current_distance, current] = queue.top();
        queue.pop();

        if (current == end) break;

        for (const auto& [neighbor, weight] : graph.at(current)) {
            Weight new_distance = current_distance + weight;
            if (!visited[neighbor].distance ||
                new_distance < visited[neighbor].distance.value()) {
                visited[neighbor] = {new_distance, current};
                queue.emplace(new_distance, neighbor);
            }
        }
    }

    if (!visited[end].distance) return {};

    // vector<T> path;
    // for (T at = end; at.has_value(); at = visited[at].parent.value_or(T{})) {
    //     path.push_back(at);
    //     if (!visited[at].parent) break;
    // }
    // reverse(path.begin(), path.end());
    // return path;
}

int main() {
    {
        WeightedGraph<string, int> graph = {
            {"A", {{"B", 1}, {"E", 3}}}, {"B", {{"A", 1}, {"C", 2}, {"D", 5}}},
            {"C", {{"A", 4}, {"D", 1}}}, {"D", {{"B", 5}, {"C", 1}}},
            {"E", {{"C", 2}}},           {"F", {}}};

        string start = "A", dest = "D";
        auto path1 = shortest_path_dijkstra(graph, start, dest);
        cout << "Path from " << start << " to " << dest << ": " << path1
             << endl;

        start = "A", dest = "F";
        auto path2 = shortest_path_dijkstra(graph, start, dest);
        cout << "Path from " << start << " to " << dest << ": " << path2
             << endl;
    }

    {
        Graph<string> graph = {{"A", {"B", "E"}}, {"B", {"A", "C"}},
                               {"C", {"A", "D"}}, {"D", {"B", "C"}},
                               {"E", {"C"}},      {"F", {}}};

        string start = "A", dest = "D";
        auto path1 = shortest_path_bfs(graph, start, dest);
        cout << "Path from " << start << " to " << dest << ": " << path1
             << endl;

        start = "A", dest = "F";
        auto path2 = shortest_path_bfs(graph, start, dest);
        cout << "Path from " << start << " to " << dest << ": " << path2
             << endl;
    }

    return 0;
}