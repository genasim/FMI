package bg.sofia.uni.fmi.mjt.fittrack.workout.filter;

import bg.sofia.uni.fmi.mjt.fittrack.workout.Workout;

public record NameWorkoutFilter(String keyword, boolean caseSensitive) implements WorkoutFilter {
    public NameWorkoutFilter(String keyword) {
        this(keyword, false);
    }

    public NameWorkoutFilter {
        if (keyword == null) {
            throw new IllegalArgumentException("[NameWorkoutFilter] Keyword cannot be null");
        }

        if (keyword.isEmpty()) {
            throw new IllegalArgumentException("[NameWorkoutFilter] Keyword cannot be an empty string");
        }
    }

    @Override
    public boolean matches(Workout workout) {
        String name = caseSensitive ? workout.getName() : workout.getName().toLowerCase();
        String token = caseSensitive ? keyword : keyword.toLowerCase();
        return name.contains(token);
    }
}
