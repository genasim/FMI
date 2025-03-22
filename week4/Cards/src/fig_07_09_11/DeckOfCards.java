package fig_07_09_11;

import java.util.Random;

public class DeckOfCards {
    private Card[] deck;
    private int currentCard;
    private final int NUMBER_OF_CARDS = 52;
    private Random randomNumbers;
    private final String faces[] = {"Ace", "Deuce", "Three", "Four", "Five", "Six",
            "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King"};
    private final String suits[] = {"Hearts", "Diamonds", "Clubs", "Spades"};


    public DeckOfCards() {
        deck = new Card[NUMBER_OF_CARDS];
        currentCard = 0;
        randomNumbers = new Random();

        for (int count = 0; count < deck.length; count++)
            deck[count] =
                    new Card(faces[count % 13], suits[count / 13]);
    }

    public void shuffle() {

        currentCard = 0;
        for (int first = 0; first < deck.length; first++) {
            int second = randomNumbers.nextInt(NUMBER_OF_CARDS);

            Card temp = deck[first];
            deck[first] = deck[second];
            deck[second] = temp;
        }
    }

    public Card dealCard() {

        if (currentCard < deck.length)
            return deck[currentCard++];
        else
            return null;
    }

    public int getHandRank(Card[] hand) {
        if (hasFourOfAKind(hand)) return 1;
        if (hasFullHouse(hand)) return 2;
        if (hasFlush(hand)) return 3;
        if (hasStraigth(hand)) return 4;
        if (hasThreeOfAKind(hand)) return 5;
        if (hasTwoPairs(hand)) return 6;
        if (hasPair(hand)) return 7;
        return 8;
    }

    public String getHandRankLaber(int rank) {
        String[] rankLabels = {
                "Four of a Kind",
                "Full House",
                "Flush",
                "Straight",
                "Three of a Kind",
                "Two Pairs",
                "Pair",
                "none",
        };

        return rankLabels[rank - 1];
    }

    private boolean hasPair(Card[] hand) {
        int[] counter = totalHand(hand);

        for (int i = 0; i < counter.length; i++) {
            if (counter[i] == 2) {
                return true;
            }
        }
        return false;
    }

    private boolean hasTwoPairs(Card[] hand) {
        int[] counters = totalHand(hand);
        int pairs = 0;

        for (int i = 0; i < counters.length; i++) {
            if (counters[i] == 2) {
                pairs++;
            }
            if (pairs == 2) {
                return true;
            }
        }
        return false;
    }

    private boolean hasThreeOfAKind(Card[] hand) {
        int[] counters = totalHand(hand);

        for (int i = 0; i < counters.length; i++) {
            if (counters[i] == 3) {
                return true;
            }
        }
        return false;
    }

    private boolean hasFourOfAKind(Card[] hand) {
        int[] counters = totalHand(hand);

        for (int i = 0; i < counters.length; i++) {
            if (counters[i] == 4) {
                return true;
            }
        }
        return false;
    }

    private boolean hasFlush(Card[] hand) {
        String suit = hand[0].getSuit();

        for (int i = 1; i < hand.length; i++) {
            if (!suit.equals(hand[i].getSuit())) {
                return false;
            }
        }
        return true;
    }

    private boolean hasStraigth(Card[] hand) {
        int[] counters = totalHand(hand);
        int consecutive = 0;

        for (int i = 0; i < counters.length; i++) {
            if (counters[i] == 1) {
                consecutive++;
            } else {
                consecutive = 0;
            }
            if (consecutive == 5) {
                return true;
            }
        }

        return consecutive == 4 && counters[0] == 1;
    }

    private boolean hasFullHouse(Card[] hand) {
        return hasPair(hand) && hasThreeOfAKind(hand);
    }

    private int[] totalHand(Card[] hand) {
        int[] counters = new int[13];
        for (int i = 0; i < hand.length; i++) {
            String face = hand[i].getFace();
            for (int j = 0; j < faces.length; j++) {
                if (face.equals(faces[j])) {
                    counters[j]++;
                }
            }
        }
        return counters;
    }
}


