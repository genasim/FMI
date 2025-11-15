package bg.sofia.uni.fmi.mjt.math;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class NumberUtilsTest {
    @Test
    void testIsPrimeOne() {
        assertFalse(NumberUtils.isPrime(1), "1 is not prime by definition");
    }

    @Test
    void testIsPrimeTwo() {
        assertTrue(NumberUtils.isPrime(2), "2 is a prime number");
    }

    @Test
    void testIsPrimeEleven() {
        assertTrue(NumberUtils.isPrime(11), "11 is a prime number");
    }

    @Test
    void testIsPrimeNegativeThrowsException() {
        assertThrows(IllegalArgumentException.class, () -> NumberUtils.isPrime(-1),
                     "Negative numbers are outside the definition domain of prime numbers and an IllegalArgumentException should be thrown");
    }

    @Test
    void testIsPrimeZeroThrowsException() {
        assertThrows(IllegalArgumentException.class, () -> NumberUtils.isPrime(0),
                     "Zero is outside the definition domain of prime numbers and an IllegalArgumentException should be thrown");
    }

    @Test
    void testIsPrimeEven() {
        assertFalse(NumberUtils.isPrime(16), "Even numbers are not prime");
    }

    @Test
    void testIsPrimeComposite() {
        assertFalse(NumberUtils.isPrime(33), "Composite numbers are not prime");
    }

    @Test
    void testIsPrime65535() {
        assertFalse(NumberUtils.isPrime(65535), "65535 is not a prime number");
    }
}
