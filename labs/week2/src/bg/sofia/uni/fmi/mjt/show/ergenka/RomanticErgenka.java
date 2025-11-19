package bg.sofia.uni.fmi.mjt.show.ergenka;

import bg.sofia.uni.fmi.mjt.show.date.DateEvent;

public class RomanticErgenka extends Participant {
    private final String favoriteDateLocation;

    public RomanticErgenka(String name, short age, int romanceLevel, int humorLevel, int rating, String favoriteDateLocation) {
        super(name, age, romanceLevel, humorLevel, rating);
        this.favoriteDateLocation = favoriteDateLocation != null ? favoriteDateLocation : "";
    }

    @Override
    public void reactToDate(DateEvent dateEvent) {
        int baseRating = (getRomanceLevel() * 7) / dateEvent.getTensionLevel() + Math.floorDiv(getHumorLevel(), 3);

        int bonus = 0;
        if (dateEvent.getLocation().equalsIgnoreCase(favoriteDateLocation)) {
            bonus += 5;
        }

        if (dateEvent.getDuration() < 30) {
            bonus -= 3;
        }

        if (dateEvent.getDuration() > 120) {
            bonus -= 2;
        }

        setRating(baseRating + bonus);
    }
}
