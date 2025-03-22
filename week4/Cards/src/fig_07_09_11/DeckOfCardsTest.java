package fig_07_09_11;

public class DeckOfCardsTest {

    public static void main(String args[]) {
        DeckOfCards myDeckOfCards = new DeckOfCards();
        myDeckOfCards.shuffle();

        Card[] rightHand = new Card[5];
        Card[] leftHand = new Card[5];

        System.out.printf("%-20s%-20s%n", "Left Hand:", "Right Hand:");
        for (int i = 0; i < leftHand.length; i++) {
            rightHand[i] = myDeckOfCards.dealCard();
            leftHand[i] = myDeckOfCards.dealCard();
            System.out.printf("%-20s%-20s%n", leftHand[i], rightHand[i]);
        }

        int leftHandRank = myDeckOfCards.getHandRank(leftHand);
        String leftHandLabel = myDeckOfCards.getHandRankLaber(leftHandRank);

        int rightHandRank = myDeckOfCards.getHandRank(rightHand);
        String rightHandLabel = myDeckOfCards.getHandRankLaber(rightHandRank);

        System.out.printf("%-20s%-20s%n", leftHandLabel, rightHandLabel);

        if (rightHandRank == leftHandRank) {
            System.out.println("\n Result: both hands are equal");
            return;
        }

        String winningHand = rightHandRank < leftHandRank ? "Right" : "Left";
        System.out.printf("%n Result: %s hand is better%n", winningHand);

//        for (int i = 0; i < 13; i++) {
//            System.out.printf("%-20s%-20s%-20s%-20s\n",
//                    myDeckOfCards.dealCard(), myDeckOfCards.dealCard(),
//                    myDeckOfCards.dealCard(), myDeckOfCards.dealCard());
//        }
    }
}
