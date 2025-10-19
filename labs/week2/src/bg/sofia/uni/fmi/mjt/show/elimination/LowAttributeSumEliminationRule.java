package bg.sofia.uni.fmi.mjt.show.elimination;

import bg.sofia.uni.fmi.mjt.show.ergenka.Ergenka;

public class LowAttributeSumEliminationRule implements EliminationRule {
    private final int threshold;

    public LowAttributeSumEliminationRule(int threshold) {
        this.threshold = threshold;
    }

    int getThreshold() {
        return threshold;
    }

    @Override
    public Ergenka[] eliminateErgenkas(Ergenka[] ergenkas) {
        if (ergenkas == null || ergenkas.length == 0) {
            return new Ergenka[0];
        }

        int length = ergenkas.length;
        for (Ergenka ergenka : ergenkas) {
            if (ergenka.getHumorLevel() + ergenka.getRomanceLevel() < getThreshold()) {
                length--;
            }
        }

        Ergenka[] remaining = new Ergenka[length];
        int idx = 0;
        for (Ergenka ergenka : ergenkas) {
            if (ergenka.getHumorLevel() + ergenka.getRomanceLevel() >= getThreshold()) {
                remaining[idx++] = ergenka;
            }
        }

        return remaining;
    }
}
