// ref: https://leetcode.com/problems/minimize-maximum-pair-sum-in-array

#include <bits/stdc++.h>
#include <math.h>

#include <vector>

using namespace std;

class Solution {
   public:
    int minPairSum(vector<int>& nums) {
        sort(nums.begin(), nums.end(), greater<int>());

        vector<pair<int, int>> pairs;
        auto startIt = nums.begin();
        auto endIt = nums.rbegin();

        for (size_t i = 0; i < nums.size() / 2; i++) {
            pairs.push_back({*startIt++, *endIt++});
        }

        int result = pairSum(pairs[0]);
        for (size_t i = 1; i < pairs.size(); i++) {
            result = max(result, pairSum(pairs[i]));
        }
        
        return result;
    }

private:
    int pairSum(const pair<int, int>& pair) {
        return pair.first + pair.second;
    }
};