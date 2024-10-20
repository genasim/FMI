// ref: https://leetcode.com/problems/minimum-absolute-difference

#include <bits/stdc++.h>
#include <math.h>

#include <vector>

using namespace std;

class Solution {
   public:
    vector<vector<int>> minimumAbsDifference(vector<int>& arr) {
        sort(arr.begin(), arr.end());

        int lowestAbsoluteDiff = INT32_MAX;
        for (size_t i = 0; i < arr.size() - 1; i++) {
            lowestAbsoluteDiff =
                min(lowestAbsoluteDiff, abs(arr[i] - arr[i + 1]));
        }

        vector<vector<int>> result;
        for (size_t i = 0; i < arr.size() - 1; i++) {
            if (abs(arr[i] - arr[i + 1]) == lowestAbsoluteDiff) {
                result.push_back({arr[i], arr[i + 1]});
            }
        }

        return result;
    }
};