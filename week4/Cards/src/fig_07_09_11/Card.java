package fig_07_09_11;

public class Card {
    private String face;
    private String suit;

    public Card(String cardFace, String cardSuit) {
        face = cardFace;
        suit = cardSuit;
    }

    public String getFace() {
        return face;
    }

    public String getSuit() {
        return suit;
    }


    public String toString() {
        return face + " of " + suit;
    }
}