package bg.sofia.uni.fmi.mjt.show.elimination;

import bg.sofia.uni.fmi.mjt.show.ergenka.Ergenka;

import java.util.Arrays;

public class PublicVoteEliminationRule implements EliminationRule {
    private final String[] votes;

    public PublicVoteEliminationRule(String[] votes) {
        this.votes = votes != null ? Arrays.copyOf(votes, votes.length) : new String[0];
    }

    String[] getVotes() {
        return Arrays.copyOf(votes, votes.length);
    }

    @Override
    public Ergenka[] eliminateErgenkas(Ergenka[] ergenkas) {
        if (ergenkas == null || ergenkas.length == 0) {
            return new Ergenka[0];
        }

        Ergenka candidate = determineMajorityCandidate(ergenkas);
        if (candidate == null) {
            return ergenkas;
        }

        Ergenka[] remaining = new Ergenka[ergenkas.length - 1];
        int ixd = 0;
        for (Ergenka ergenka : ergenkas) {
            if (!ergenka.getName().equals(candidate.getName())) {
                remaining[ixd++] = ergenka;
            }
        }
        return remaining;
    }

    private Ergenka determineMajorityCandidate(Ergenka[] ergenkas) {
        String majorityVote = null;
        int counter = 0;
        for (String vote : votes) {
            if (vote == null) {
                continue;
            }

            if (counter == 0) {
                majorityVote = vote;
                counter++;
                continue;
            }

            if (majorityVote.equals(vote)) {
                counter++;
            } else {
                counter--;
            }
        }

        int count = 0;
        for (String vote : votes) {
            if (vote.equals(majorityVote)) {
                count++;
            }
        }
        if (count <= votes.length / 2) {
            return null;
        }

        for (Ergenka ergenka : ergenkas) {
            if (ergenka.getName().equals(majorityVote)) {
                return ergenka;
            }
        }
        return null;
    }
}
