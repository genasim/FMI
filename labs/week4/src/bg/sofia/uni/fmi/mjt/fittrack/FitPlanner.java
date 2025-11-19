package bg.sofia.uni.fmi.mjt.fittrack;

import bg.sofia.uni.fmi.mjt.fittrack.exception.OptimalPlanImpossibleException;
import bg.sofia.uni.fmi.mjt.fittrack.workout.Workout;
import bg.sofia.uni.fmi.mjt.fittrack.workout.WorkoutType;
import bg.sofia.uni.fmi.mjt.fittrack.workout.comparator.WorkoutCaloriesComparator;
import bg.sofia.uni.fmi.mjt.fittrack.workout.comparator.WorkoutDifficultyComparator;
import bg.sofia.uni.fmi.mjt.fittrack.workout.filter.WorkoutFilter;

import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class FitPlanner implements FitPlannerAPI, Iterable<Workout> {
    private final HashSet<Workout> workouts;

    public FitPlanner(Collection<Workout> availableWorkouts) {
        if (availableWorkouts == null) {
            throw new IllegalArgumentException("Cannot create FitPlanner with null workouts");
        }
        this.workouts = new HashSet<>(availableWorkouts);
    }

    @Override
    public List<Workout> generateOptimalWeeklyPlan(int totalMinutes)
        throws OptimalPlanImpossibleException {
        if (totalMinutes < 0) {
            throw new IllegalArgumentException("Cannot generate optimal weekly plan based on negative minutes");
        }

        if (workouts.isEmpty() || totalMinutes == 0) {
            return List.of();
        }

        if (!canFitAnyWorkout(workouts, totalMinutes)) {
            throw new OptimalPlanImpossibleException("Cannot generate optimal weekly plan");
        }

        List<Workout> selectedWorkouts = generateOptimalWorkoutsList(totalMinutes);

        selectedWorkouts.sort(
            new WorkoutCaloriesComparator().reversed()
                .thenComparing(new WorkoutDifficultyComparator().reversed()));

        return List.copyOf(selectedWorkouts);
    }

    @Override
    public List<Workout> findWorkoutsByFilters(List<WorkoutFilter> filters) {
        if (filters == null) {
            throw new IllegalArgumentException("Cannot filter workouts based on null filters");
        }

        if (workouts.isEmpty()) {
            return new ArrayList<>();
        }

        if (filters.isEmpty()) {
            return new ArrayList<>(workouts);
        }

        List<Workout> results = new ArrayList<>();
        for (Workout workout : workouts) {
            boolean matchAll = true;
            for (WorkoutFilter filter : filters) {
                if (!filter.matches(workout)) {
                    matchAll = false;
                    break;
                }
            }
            if (matchAll) {
                results.add(workout);
            }
        }

        return results;
    }

    @Override
    public Map<WorkoutType, List<Workout>> getWorkoutsGroupedByType() {
        if (workouts.isEmpty()) {
            return Map.of();
        }

        Map<WorkoutType, List<Workout>> result = new EnumMap<>(WorkoutType.class);
        for (Workout workout : workouts) {
            result.putIfAbsent(workout.getType(), new LinkedList<>());
            result.get(workout.getType()).add(workout);
        }
        return Map.copyOf(result);
    }

    @Override
    public List<Workout> getWorkoutsSortedByCalories() {
        if (workouts.isEmpty()) {
            return List.of();
        }

        List<Workout> result = new ArrayList<>(workouts);
        result.sort(new WorkoutCaloriesComparator().reversed());
        return List.copyOf(result);
    }

    @Override
    public List<Workout> getWorkoutsSortedByDifficulty() {
        if (workouts.isEmpty()) {
            return List.of();
        }

        List<Workout> result = new ArrayList<>(workouts);
        result.sort(new WorkoutDifficultyComparator());
        return List.copyOf(result);
    }

    @Override
    public Set<Workout> getUnmodifiableWorkoutSet() {
        if (workouts.isEmpty()) {
            return Set.of();
        }

        return Set.copyOf(workouts);
    }

    @Override
    public Iterator<Workout> iterator() {
        return workouts.iterator();
    }

    private boolean canFitAnyWorkout(Collection<Workout> workouts, int totalMinutes) {
        boolean canFitAnyWorkout = false;
        for (Workout workout : workouts) {
            if (workout.getDuration() <= totalMinutes) {
                canFitAnyWorkout = true;
                break;
            }
        }
        return canFitAnyWorkout;
    }

    private List<Workout> generateOptimalWorkoutsList(int totalMinutes) {
        List<Workout> workoutList = new ArrayList<>(workouts);
        int n = workoutList.size();
        int[][] dp = new int[n + 1][totalMinutes + 1];

        for (int i = 1; i <= n; i++) {
            Workout currentWorkout = workoutList.get(i - 1);
            int duration = currentWorkout.getDuration();
            int calories = currentWorkout.getCaloriesBurned();

            for (int w = 0; w <= totalMinutes; w++) {
                dp[i][w] = dp[i - 1][w];
                if (duration <= w) {
                    dp[i][w] = Math.max(dp[i][w], dp[i - 1][w - duration] + calories);
                }
            }
        }

        List<Workout> selectedWorkouts = new ArrayList<>();
        int w = totalMinutes;
        for (int i = n; i > 0 && w > 0; i--) {
            if (dp[i][w] != dp[i - 1][w]) {
                Workout workout = workoutList.get(i - 1);
                selectedWorkouts.add(workout);
                w -= workout.getDuration();
            }
        }

        return selectedWorkouts;
    }
}
