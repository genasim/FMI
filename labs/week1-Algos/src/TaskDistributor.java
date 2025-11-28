public class TaskDistributor {
    public static int minDifference(int[] tasks) {
        if (tasks == null || tasks.length == 0) {
            return 0;
        }

        if (tasks.length == 1) {
            return tasks[0];
        }

        int totalTime = 0;
        for (int i = 0; i < tasks.length; i++) {
            totalTime += tasks[i];
        }
        int targetTime = totalTime / 2;

        boolean[] dp = new boolean[targetTime + 1];
        dp[0] = true;

        for (int i = 0; i < tasks.length; i++) {
            for (int j = targetTime; j >= tasks[i]; j--) {
                if (dp[j - tasks[i]]) {
                    dp[j] = true;
                }
            }
        }

        int best = 0;
        for (int i = targetTime; i >= 0; i--) {
            if (dp[i]) {
                best = i;
                break;
            }
        }

        return totalTime - 2 * best;
    }

    static void main() {
        System.out.printf("{1, 2, 3, 4, 5}: %s \n", minDifference(new int[] {1, 2, 3, 4, 5}));
        System.out.printf("{10, 20, 15, 5}: %s \n", minDifference(new int[] {10, 20, 15, 5}));
        System.out.printf("{7, 3, 2, 1, 5, 4}: %s \n", minDifference(new int[] {7, 3, 2, 1, 5, 4}));
        System.out.printf("{9, 1, 1, 1}: %s \n", minDifference(new int[] {9, 1, 1, 1}));
        System.out.printf("{}: %s \n", minDifference(new int[] {}));
        System.out.printf("{120}: %s \n", minDifference(new int[] {120}));
        System.out.printf("{30, 30}: %s \n", minDifference(new int[] {30, 30}));
    }
}
