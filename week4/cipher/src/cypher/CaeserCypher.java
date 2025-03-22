package cypher;

public class CaeserCypher {
    private int shiftLength;

    private final int ALPHABET_SIZE = 26;
    private final char CAPITAL_LETTER_A = 'A';

    public CaeserCypher(int shiftLength) {
        setShiftLength(shiftLength);
    }

    public int getShiftLength() {
        return shiftLength;
    }

    public void setShiftLength(int shiftLength) {
        this.shiftLength = shiftLength > 0 ? shiftLength % ALPHABET_SIZE : 3;
    }

    public String encrypt(String plaintext) {
        char[] symbols = plaintext.toCharArray();

        for (int i = 0; i < symbols.length; i++)
            symbols[i] = (char) ((symbols[i] + shiftLength - CAPITAL_LETTER_A) % 26 + CAPITAL_LETTER_A);

        return new String(symbols);
    }

    public String decrypt(String cypherText) {
        char[] symbols = cypherText.toCharArray();

        for (int i = 0; i < symbols.length; i++)
            symbols[i] = (char) ((symbols[i] - shiftLength + ALPHABET_SIZE - CAPITAL_LETTER_A) % 26 + CAPITAL_LETTER_A);

        return new String(symbols);
    }
}
