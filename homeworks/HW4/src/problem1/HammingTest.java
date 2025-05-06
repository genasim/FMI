package problem1;

import java.util.Scanner;

public class HammingTest {
    private static final int MAX_BIT_LENGTH = 8;
    private static final Scanner scanner = new Scanner(System.in);

    public static void main(String[] args) {
        var calculator = new HammingBitCalculator(MAX_BIT_LENGTH);

        System.out.println("Hamming distance calculator");
        System.out.print("Enter a: ");
        int a = scanner.nextInt();
        System.out.print("Enter b: ");
        int b = scanner.nextInt();

        int distance = calculator.distanceBetween(a, b);

        System.out.printf("%d: %s \n%d: %s \nHamming distance: %d\n",
                a, toBinaryString(a, MAX_BIT_LENGTH),
                b, toBinaryString(b, MAX_BIT_LENGTH),
                distance);
    }

    private static String toBinaryString(int num, int length) {
        StringBuilder sb = new StringBuilder();
        int mask = 1;
        for (int i = 0; i < length; i++) {
            sb.append(mask & num);
            num >>= 1;
        }
        return sb.reverse().toString();
    }
}
