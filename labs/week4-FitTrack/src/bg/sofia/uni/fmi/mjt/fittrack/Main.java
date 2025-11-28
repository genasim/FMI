package bg.sofia.uni.fmi.mjt.fittrack;

import bg.sofia.uni.fmi.mjt.fittrack.workout.CardioWorkout;
import bg.sofia.uni.fmi.mjt.fittrack.workout.StrengthWorkout;
import bg.sofia.uni.fmi.mjt.fittrack.workout.Workout;
import bg.sofia.uni.fmi.mjt.fittrack.workout.WorkoutType;
import bg.sofia.uni.fmi.mjt.fittrack.workout.YogaSession;
import bg.sofia.uni.fmi.mjt.fittrack.workout.filter.CaloriesWorkoutFilter;
import bg.sofia.uni.fmi.mjt.fittrack.workout.filter.NameWorkoutFilter;
import bg.sofia.uni.fmi.mjt.fittrack.workout.filter.TypeWorkoutFilter;

import java.util.Arrays;
import java.util.List;

public class Main {
    static void main() {
        List<Workout> workouts = Arrays.asList(
            new CardioWorkout("HIIT", 30, 400, 4),
            new StrengthWorkout("Upper Body", 45, 350, 3),
            new YogaSession("Morning Flow", 20, 150, 2),
            new CardioWorkout("Cycling", 60, 600, 5),
            new StrengthWorkout("Leg Day", 30, 250, 2),
            new YogaSession("Evening Relax", 15, 100, 1),
            new YogaSession("Morning Relax", 15, 100, 1)
        );

        FitPlanner planner = new FitPlanner(workouts);

        for (Workout workout : planner) {
            System.out.println(workout);
        }

        System.out.println(planner.findWorkoutsByFilters(List.of()));
        System.out.println(planner.findWorkoutsByFilters(List.of(
            new CaloriesWorkoutFilter(0, 450),
            new TypeWorkoutFilter(WorkoutType.YOGA),
            new NameWorkoutFilter("relax")
        )));

        System.out.println("UnmodifiableWorkoutSet");
        System.out.println(planner.getUnmodifiableWorkoutSet());
        System.out.println();

        System.out.println("Workouts grouped by Type");
        System.out.println(planner.getWorkoutsGroupedByType());
        System.out.println();

        System.out.println("Workouts sorted by calories");
        System.out.println(planner.getWorkoutsSortedByCalories());
        System.out.println();

        System.out.println("Workouts sorted by difficulty");
        System.out.println(planner.getWorkoutsSortedByDifficulty());
    }
}
