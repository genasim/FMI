package bg.sofia.uni.fmi.mjt.math;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class CalcTest {
    private Calc calc = new Calc();

    @Test
    void testMultiplyFirstArgZero() {
        assertEquals(0, calc.multiply(0, 10), "Multiplying zero by any number should return zero");
    }

    @Test
    void testMultiplySecondArgZero() {
        assertEquals(0, calc.multiply(10, 0), "Multiplying a number by zero should return zero");
    }

    @Test
    void testMultiplyBothArgsZero() {
        assertEquals(0, calc.multiply(0, 0), "Multiplying zero by zero should return zero");
    }
}
