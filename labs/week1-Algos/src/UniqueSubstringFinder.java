import java.util.Arrays;

public class UniqueSubstringFinder {
    public static String longestUniqueSubstring(String s) {
        if (s == null || s.isEmpty()) {
            return "";
        }

        int[] lastPos = new int[26];
        Arrays.fill(lastPos, -1);

        int bestStart = 0, bestLen = 0, start = 0;

        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            int index = c - 'a';

            int prev = lastPos[index];

            if (prev >= start) {
                start = prev + 1;
            }

            lastPos[index] = i;

            int currentLen = i - start + 1;
            if (currentLen > bestLen) {
                bestLen = currentLen;
                bestStart = start;
            }
        }

        return s.substring(bestStart, bestStart + bestLen);
    }

    static void main() {
        System.out.printf("abcabcbb: %s \n", longestUniqueSubstring("abcabcbb"));
        System.out.printf("bbbbb: %s \n", longestUniqueSubstring("bbbbb"));
        System.out.printf("pwwkew: %s \n", longestUniqueSubstring("pwwkew"));
        System.out.printf("abcdefg: %s \n", longestUniqueSubstring("abcdefg"));
        System.out.printf("x: %s \n", longestUniqueSubstring("x"));
        System.out.printf(": %s \n", longestUniqueSubstring(""));
    }
}
