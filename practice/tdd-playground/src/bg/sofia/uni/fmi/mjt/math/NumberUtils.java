package bg.sofia.uni.fmi.mjt.math;

public class NumberUtils {
    public static boolean isPrime(int n) {
        if (n <= 0) {
            throw new IllegalArgumentException("Non-positive numbers are out of DD of primes");
        }

        if (n == 2) {
            return true;
        }

        if (n == 1 || n % 2 == 0) {
            return false;
        }

        for (int i = (int) Math.sqrt(n); i >= 3; i -= 2) {
            if (n % i == 0) {
                return false;
            }
        }
        return true;
    }
}
