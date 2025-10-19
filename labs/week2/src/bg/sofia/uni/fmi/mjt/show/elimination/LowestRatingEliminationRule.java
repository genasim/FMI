package bg.sofia.uni.fmi.mjt.show.elimination;

import bg.sofia.uni.fmi.mjt.show.ergenka.Ergenka;

public class LowestRatingEliminationRule implements EliminationRule {
    @Override
    public Ergenka[] eliminateErgenkas(Ergenka[] ergenkas) {
        if (ergenkas == null || ergenkas.length == 0) {
            return new Ergenka[0];
        }

        int length = ergenkas.length - 1;
        int lowestRating = ergenkas[0].getRating();
        for (int i = 1; i < ergenkas.length; i++) {
            if (lowestRating > ergenkas[i].getRating()) {
                lowestRating = ergenkas[i].getRating();
                continue;
            }

            if (lowestRating == ergenkas[i].getRating()) {
                length--;
            }
        }

        Ergenka[] remaining = new Ergenka[length];
        int idx = 0;
        for (Ergenka ergenka : ergenkas) {
            if (ergenka.getRating() > lowestRating) {
                remaining[idx++] = ergenka;
            }
        }

        return remaining;
    }
}
