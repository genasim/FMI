package problem1;

public class HammingBitCalculator {
    private int bitLength;

    HammingBitCalculator(int bitLength) {
        setBitLength(bitLength);
    }

    public int getBitLength() {
        return bitLength;
    }

    public void setBitLength(int bitLength) {
        this.bitLength = Math.max(bitLength, 0);
    }

    public int distanceBetween(int a, int b) {
        if (isInvalid(a) || isInvalid(b)) {
            return -1;
        }

        int distance = 0;
        while (a > 0 || b > 0) {
            // Extract the right-most bits, then XOR them:
            // if different then result is 1 => increment distance; if same, then the result is 0 => distance unchanged
            distance += (a & 1) ^ (b & 1);
            a >>= 1;
            b >>= 1;
        }
        return distance;
    }

    private boolean isInvalid(int a) {
        return a < -Math.pow(2, getBitLength() - 1) || a > Math.pow(2, getBitLength() - 1) - 1;
    }
}
