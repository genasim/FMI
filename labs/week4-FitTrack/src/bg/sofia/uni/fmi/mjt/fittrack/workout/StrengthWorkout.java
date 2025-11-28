package bg.sofia.uni.fmi.mjt.fittrack.workout;

public non-sealed class StrengthWorkout extends BaseWorkout {
    public StrengthWorkout(String name, int duration, int caloriesBurned, int difficulty) {
        super(name, duration, caloriesBurned, difficulty, WorkoutType.STRENGTH);
    }
}
