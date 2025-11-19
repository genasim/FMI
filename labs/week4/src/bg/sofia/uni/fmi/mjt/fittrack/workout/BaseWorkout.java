package bg.sofia.uni.fmi.mjt.fittrack.workout;

import bg.sofia.uni.fmi.mjt.fittrack.exception.InvalidWorkoutException;

import java.util.Objects;

public abstract sealed class BaseWorkout implements Workout permits CardioWorkout, StrengthWorkout, YogaSession {
    private static final int MIN_DURATION = 0;
    private static final int MIN_CALORIES_BURNT = 0;
    private static final int MIN_DIFFICULTY = 1;
    private static final int MAX_DIFFICULTY = 5;

    private final String name;
    private final int duration;
    private final int caloriesBurnt;
    private final int difficulty;
    private final WorkoutType type;

    public BaseWorkout(String name, int duration, int caloriesBurned, int difficulty, WorkoutType type) {
        if (name == null || name.trim().isEmpty()) {
            throw new InvalidWorkoutException("Workout name cannot be null or empty");
        }

        if (duration <= MIN_DURATION) {
            throw new InvalidWorkoutException("Workout duration must be positive");
        }

        if (caloriesBurned <= MIN_CALORIES_BURNT) {
            throw new InvalidWorkoutException("Workout calories burned must be positive");
        }

        if (difficulty < MIN_DIFFICULTY || difficulty > MAX_DIFFICULTY) {
            throw new InvalidWorkoutException("Workout difficulty must be between 1 and 5");
        }

        this.name = name;
        this.duration = duration;
        this.caloriesBurnt = caloriesBurned;
        this.difficulty = difficulty;
        this.type = type;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public int getDuration() {
        return duration;
    }

    @Override
    public int getCaloriesBurned() {
        return caloriesBurnt;
    }

    @Override
    public int getDifficulty() {
        return difficulty;
    }

    @Override
    public WorkoutType getType() {
        return type;
    }

    @Override
    public boolean equals(Object o) {
        if (o == null || getClass() != o.getClass()) return false;

        BaseWorkout that = (BaseWorkout) o;
        return getDuration() == that.getDuration() && caloriesBurnt == that.caloriesBurnt &&
            getDifficulty() == that.getDifficulty() && getName().equals(that.getName()) && getType() == that.getType();
    }

    @Override
    public int hashCode() {
        int result = getName().hashCode();
        result = 31 * result + getDuration();
        result = 31 * result + caloriesBurnt;
        result = 31 * result + getDifficulty();
        result = 31 * result + Objects.hashCode(getType());
        return result;
    }

    @Override
    public String toString() {
        return String.format("%s[name=%s, duration=%d, calories=%d, difficulty=%d]", getType(), getName(),
                             getDuration(), getCaloriesBurned(), getDifficulty());
    }
}
