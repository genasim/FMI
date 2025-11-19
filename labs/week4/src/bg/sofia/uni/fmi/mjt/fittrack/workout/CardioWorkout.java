package bg.sofia.uni.fmi.mjt.fittrack.workout;

public non-sealed class CardioWorkout extends BaseWorkout {
    public CardioWorkout(String name, int duration, int caloriesBurned, int difficulty) {
        super(name, duration, caloriesBurned, difficulty, WorkoutType.CARDIO);
    }
}
