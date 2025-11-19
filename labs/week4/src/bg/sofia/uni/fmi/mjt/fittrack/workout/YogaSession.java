package bg.sofia.uni.fmi.mjt.fittrack.workout;

public non-sealed class YogaSession extends BaseWorkout {
    public YogaSession(String name, int duration, int caloriesBurned, int difficulty) {
        super(name, duration, caloriesBurned, difficulty, WorkoutType.YOGA);
    }
}
