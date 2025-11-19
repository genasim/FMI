package bg.sofia.uni.fmi.mjt.show;

import bg.sofia.uni.fmi.mjt.show.date.DateEvent;
import bg.sofia.uni.fmi.mjt.show.elimination.EliminationRule;
import bg.sofia.uni.fmi.mjt.show.elimination.LowAttributeSumEliminationRule;
import bg.sofia.uni.fmi.mjt.show.elimination.LowestRatingEliminationRule;
import bg.sofia.uni.fmi.mjt.show.elimination.PublicVoteEliminationRule;
import bg.sofia.uni.fmi.mjt.show.ergenka.Ergenka;
import bg.sofia.uni.fmi.mjt.show.ergenka.HumorousErgenka;
import bg.sofia.uni.fmi.mjt.show.ergenka.RomanticErgenka;

import java.util.Arrays;

public class ShowAPIImpl implements ShowAPI {
    private Ergenka[] ergenkas;
    private final EliminationRule[] defaultEliminationRules;

    public ShowAPIImpl(Ergenka[] ergenkas, EliminationRule[] defaultEliminationRules) {
        this.ergenkas = ergenkas;
        this.defaultEliminationRules =
            defaultEliminationRules != null && defaultEliminationRules.length != 0 ? defaultEliminationRules :
                new EliminationRule[] {new LowestRatingEliminationRule()};
    }

    @Override
    public Ergenka[] getErgenkas() {
        return ergenkas;
    }

    @Override
    public void playRound(DateEvent dateEvent) {
        for (Ergenka ergenka : ergenkas) {
            organizeDate(ergenka, dateEvent);
        }
    }

    @Override
    public void eliminateErgenkas(EliminationRule[] eliminationRules) {
        if (ergenkas.length == 0) {
            return;
        }

        if (eliminationRules == null || eliminationRules.length == 0) {
            eliminationRules = defaultEliminationRules;
        }

        for (EliminationRule eliminationRule : eliminationRules) {
            ergenkas = eliminationRule.eliminateErgenkas(ergenkas);
        }
    }

    @Override
    public void organizeDate(Ergenka ergenka, DateEvent dateEvent) {
        ergenka.reactToDate(dateEvent);
    }

    public static void main(String... args) {
//        Ergenka[] bachelorettes = new Ergenka[]{
//                new HumorousErgenka("Georgia", (short) 21, 2, 4, 3),
//                new RomanticErgenka("Geri", (short) 20, 6, 3, 3, "Mladost"),
//                new RomanticErgenka("Maria", (short) 22, 4, 3, 4, "Kazichene"),
//                new HumorousErgenka("Anna-maria", (short) 19, 2, 4, 3)
//        };
//        EliminationRule[] defaultEliminationRules = new EliminationRule[]{
//                new LowestRatingEliminationRule(),
//        };
//
//        BachelorShow bachelorShow = new BachelorShow(bachelorettes, defaultEliminationRules);
//
//        DateEvent date = new DateEvent("Kazichene", 4, 40);
//        bachelorShow.playRound(date);
//        EliminationRule[] eliminationRules = new EliminationRule[]{
//                new LowestRatingEliminationRule(),
//                new LowAttributeSumEliminationRule(8),
//        };
//        bachelorShow.eliminateErgenkas(eliminationRules);

        {
            Ergenka[] ergs = {
                new HumorousErgenka("Maria", (short) 21, 3, 5, 2),
                new RomanticErgenka("Sofia", (short) 22, 6, 2, 4, "Paris"),
                new HumorousErgenka("Marta", (short) 23, 4, 4, 1)
            };
            EliminationRule[] defaults = {new LowestRatingEliminationRule()};
            ShowAPI show = new ShowAPIImpl(ergs, defaults);

            show.eliminateErgenkas(null);
            System.out.println("Case 1 (null rules): " +
                                   Arrays.toString(Arrays.stream(show.getErgenkas()).map(Ergenka::getName).toArray()));

            show.eliminateErgenkas(new EliminationRule[0]);
            System.out.println("Case 1 (empty rules): " +
                                   Arrays.toString(Arrays.stream(show.getErgenkas()).map(Ergenka::getName).toArray()));
        }
        System.out.println("==============================");
        {
            Ergenka[] ergs = {
                new HumorousErgenka("Ginka", (short) 20, 2, 3, 5),
                new RomanticErgenka("Ivanka", (short) 21, 7, 1, 2, "Rome"),
                new HumorousErgenka("Debora", (short) 22, 1, 2, 2) // tie with R1
            };
            ShowAPI show = new ShowAPIImpl(ergs, new EliminationRule[] {new LowestRatingEliminationRule()});

            show.eliminateErgenkas(null);
            System.out.println("Case 2: " + Arrays.toString(
                Arrays.stream(show.getErgenkas()).map(Ergenka::getName).toArray())); // expect ["H1"]
        }
        System.out.println("==============================");
        {
            Ergenka[] ergs = {
                new HumorousErgenka("Desi", (short) 24, 5, 5, 4), // sum = 8
                new RomanticErgenka("Katya", (short) 25, 2, 5, 3, "Sofia"), // sum = 7
                new HumorousErgenka("Masha", (short) 26, 4, 4, 4), // sum = 8
                new RomanticErgenka("Neli", (short) 25, 1, 3, 3, "Sofia"), // sum = 7
            };
            ShowAPI show = new ShowAPIImpl(ergs, null);

            show.eliminateErgenkas(new EliminationRule[] {new LowAttributeSumEliminationRule(8)});
            System.out.println("Case 3 (t=8): " + Arrays.toString(
                Arrays.stream(show.getErgenkas()).map(Ergenka::getName).toArray())); // expect ["H1","H2"]
        }
        System.out.println("==============================");
        {
            Ergenka[] ergs = {
                new HumorousErgenka("Leti", (short) 20, 3, 7, 4),
                new RomanticErgenka("Mila", (short) 21, 8, 2, 4, "Paris"),
                new HumorousErgenka("Nadya", (short) 22, 3, 6, 4)
            };
            ShowAPI show = new ShowAPIImpl(ergs, null);

            String[] votes = {"Petya", "Petya", "Mila", "Petya", "Nadya"};
            show.eliminateErgenkas(new EliminationRule[] {new PublicVoteEliminationRule(votes)});
            System.out.println(
                "Case 4: " + Arrays.toString(Arrays.stream(show.getErgenkas()).map(Ergenka::getName).toArray()));
        }
        System.out.println("==============================");
        {
            Ergenka[] ergs = {
                new HumorousErgenka("H1", (short) 20, 3, 7, 4),
                new RomanticErgenka("R1", (short) 21, 8, 2, 4, "Paris"),
                new HumorousErgenka("H2", (short) 22, 3, 6, 4)
            };
            String[] votes = {"R1", "H2", "H1", "R1", "H2"}; // R1=2, H2=2, H1=1 → no 50%+1
            ShowAPI show = new ShowAPIImpl(ergs, null);

            Ergenka[] before = show.getErgenkas();
            show.eliminateErgenkas(new EliminationRule[] {new PublicVoteEliminationRule(votes)});
            System.out.println("Case 5 (unchanged): " + Arrays.equals(before, show.getErgenkas()));
        }
        System.out.println("==============================");
        {
            Ergenka[] ergs = {
                new HumorousErgenka("H29", (short) 20, 3, 6, 10), // duration 29 → -2 bonus
                new HumorousErgenka("H30", (short) 20, 3, 6, 10), // duration 30 → +4 bonus
                new HumorousErgenka("H90", (short) 20, 3, 6, 10), // duration 90 → +4 bonus
                new HumorousErgenka("H91", (short) 20, 3, 6, 10)  // duration 91 → -3 bonus
            };
            ShowAPI show = new ShowAPIImpl(ergs, null);

            show.organizeDate(ergs[0], new DateEvent("X", 5, 29));
            show.organizeDate(ergs[1], new DateEvent("X", 5, 30));
            show.organizeDate(ergs[2], new DateEvent("X", 5, 90));
            show.organizeDate(ergs[3], new DateEvent("X", 5, 91));

            System.out.println("Case 6 ratings: " + Arrays.toString(
                Arrays.stream(show.getErgenkas()).map(Ergenka::getRating).toArray()));
            // Expect H30 and H90 > H29 and H91 due to +4 vs -2/-3 bonuses (exact deltas depend on your formula impl)
        }

    }
}
