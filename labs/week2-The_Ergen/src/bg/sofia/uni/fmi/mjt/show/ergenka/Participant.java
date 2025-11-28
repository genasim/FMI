package bg.sofia.uni.fmi.mjt.show.ergenka;

public abstract class Participant implements Ergenka {
    private final String name;
    private final short age;
    private final int romanceLevel;
    private final int humorLevel;
    protected int rating;

    Participant(String name, short age, int romanceLevel, int humorLevel, int rating) {
        this.name = name != null ? name : "";
        this.age = age;
        this.romanceLevel = romanceLevel;
        this.humorLevel = humorLevel;
        setRating(rating);
    }

    protected void setRating(int rating) {
        this.rating = rating;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public short getAge() {
        return age;
    }

    @Override
    public int getRomanceLevel() {
        return romanceLevel;
    }

    @Override
    public int getHumorLevel() {
        return humorLevel;
    }

    @Override
    public int getRating() {
        return rating;
    }
}
