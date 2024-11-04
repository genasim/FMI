// ref: https://leetcode.com/problems/rearrange-array-to-maximize-prefix-score

#include <bits/stdc++.h>
#include <math.h>

#include <vector>

using namespace std;

class Solution {
   public:
    int maxScore(vector<int>& nums) {
        sort(nums.begin(), nums.end(), greater<int>());
        if (nums[0] <= 0) return 0;

        int result = 0, sum = 0;
        for (auto&& num : nums) {
            sum += num;
            if (sum <= 0) break;
            result++;
        }

        return result;
    }
};

#include <fstream>

ostream& operator<<(ostream& os, const vector<int>& arr) {
    for (auto&& num : arr) {
        os << num << ' ';
    }
    return os;
}

int main() {
    vector<int> arr = {2,-1,0,1,-3,3,-3};
    sort(arr.begin(), arr.end(), greater<int>());
    cout << arr << endl;

    return 0;
}