// ref: https://leetcode.com/problems/maximum-units-on-a-truck/description

#include <bits/stdc++.h>

#include <vector>

using namespace std;

bool compareBoxTypes(const vector<int> &a, const vector<int> &b) { return a[1] > b[1]; }

class Solution {
   public:
    int maximumUnits(vector<vector<int>>& boxTypes, int truckSize) {
        sort(boxTypes.begin(), boxTypes.end(), compareBoxTypes);

        int totalUnits = 0;
        for (const vector<int> &box : boxTypes) {
            uint32_t boxesToTake = min(box[0], truckSize);
            totalUnits += boxesToTake * box[1];

            truckSize -= boxesToTake;
            if (truckSize <= 0) {
                break;
            } 
        }
        

        return totalUnits;
    }
};