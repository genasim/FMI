// ref: https://leetcode.com/problems/minimum-number-game/description

#include <bits/stdc++.h>

#include <vector>

using namespace std;

class Solution {
   public:
    vector<int> numberGame(const vector<int>& nums) {
        vector<int> result = nums;

        sort(result.begin(), result.end());
        for (size_t i = 0; i < result.size(); i += 2) {
            std::swap(result[i], result[i + 1]);
        }

        return result;
    }
};