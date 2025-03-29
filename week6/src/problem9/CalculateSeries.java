package problem9;

public class CalculateSeries {
    public static double calculateSeries(int i) {
        if (i <= 0) throw new IllegalArgumentException();

        if (i == 1) return 1 / 3.0;
        return (double) i / (2 * i + 1) + calculateSeries(i - 1);
    }

    public static void main(String[] args) {
        for (int i = 1; i <= 10; i++) {
            System.out.printf("%d: %.10f%n", i, calculateSeries(i));
        }
    }
}
