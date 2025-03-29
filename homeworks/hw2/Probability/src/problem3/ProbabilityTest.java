package problem3;

import java.text.NumberFormat;

public class ProbabilityTest {
    public static void main(String[] args) {
        accumulate(10000);
        accumulate(60000);
    }

    private static int drawRandomNumber() {
        double r = Math.random();
        if (r < 0.2) {
            return 1;
        } else if (r < 0.5) {
            return 2;
        } else {
            return 3;
        }
    }

    private static void accumulate(int iterations) {
        NumberFormat percentFormat = NumberFormat.getPercentInstance();
        percentFormat.setMinimumFractionDigits(2);

        int count1 = 0, count2 = 0, count3 = 0;

        for (int i = 0; i < iterations; i++) {
            int result = drawRandomNumber();
            if (result == 1) {
                count1++;
            } else if (result == 2) {
                count2++;
            } else if (result == 3) {
                count3++;
            }
        }

        double prob1 = (double) count1 / iterations;
        double prob2 = (double) count2 / iterations;
        double prob3 = (double) count3 / iterations;

        System.out.println("After " + iterations + " iterations:");
        System.out.println("P(1) = " + percentFormat.format(prob1));
        System.out.println("P(2) = " + percentFormat.format(prob2));
        System.out.println("P(3) = " + percentFormat.format(prob3));
        System.out.println();
    }
}
