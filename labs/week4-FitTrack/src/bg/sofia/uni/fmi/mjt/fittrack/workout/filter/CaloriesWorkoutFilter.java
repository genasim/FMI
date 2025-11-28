package bg.sofia.uni.fmi.mjt.fittrack.workout.filter;

import bg.sofia.uni.fmi.mjt.fittrack.workout.Workout;

public record CaloriesWorkoutFilter(int min, int max) implements WorkoutFilter {
    public CaloriesWorkoutFilter {
        if (min < 0 || max < 0) {
            throw new IllegalArgumentException(
                "[CaloriesWorkoutFilter]: Invalid min/max boundaries; min and max cannot be negative");
        }

        if (min > max) {
            throw new IllegalArgumentException(
                "[CaloriesWorkoutFilter]: Invalid min/max boundaries; min cannot be greater than max");
        }
    }

    @Override
    public boolean matches(Workout workout) {
        return min <= workout.getCaloriesBurned() && workout.getCaloriesBurned() <= max;
    }
}
