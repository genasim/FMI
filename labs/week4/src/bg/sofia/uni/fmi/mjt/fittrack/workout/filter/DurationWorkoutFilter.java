package bg.sofia.uni.fmi.mjt.fittrack.workout.filter;

import bg.sofia.uni.fmi.mjt.fittrack.workout.Workout;

public record DurationWorkoutFilter(int min, int max) implements WorkoutFilter {
    public DurationWorkoutFilter {
        if (min < 0 || max < 0) {
            throw new IllegalArgumentException(
                "[DurationWorkoutFilter]: Invalid min/max boundaries; min and max cannot be negative");
        }

        if (min > max) {
            throw new IllegalArgumentException(
                "[DurationWorkoutFilter]: Invalid min/max boundaries; min cannot be greater than max");
        }
    }

    @Override
    public boolean matches(Workout workout) {
        return min <= workout.getDuration() && workout.getDuration() <= max;
    }
}
