// ref: https://leetcode.com/problems/sort-array-by-parity

#include <bits/stdc++.h>
#include <math.h>

#include <vector>

using namespace std;

class Solution {
   public:
    vector<int> sortArrayByParity(vector<int>& nums) {
        vector<int> result(nums.size());
        uint32_t evensCount = std::count_if(
            nums.cbegin(), nums.cend(), [](int num) { return num % 2 == 0; });

        auto evenIt = result.begin();
        auto oddIt = result.begin() + evensCount;

        for (auto&& num : nums) {
            if (num % 2 == 0)
                *evenIt++ = num;
            else
                *oddIt++ = num;
        }

        return result;
    }
};