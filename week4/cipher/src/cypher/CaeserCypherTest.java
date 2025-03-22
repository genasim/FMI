package cypher;

public class CaeserCypherTest {
    public static void main(String[] args) {
        CaeserCypher cypher = new CaeserCypher(6);

        String plaintext = "HELLO WORLD";
        String encrypted = cypher.encrypt(plaintext);

        System.out.println(plaintext);
        System.out.printf("Encrypted: %s\n", encrypted);
        System.out.printf("Decrypted: %s\n", cypher.decrypt(encrypted));
    }
}
