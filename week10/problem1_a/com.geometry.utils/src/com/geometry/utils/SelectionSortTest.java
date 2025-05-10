package com.geometry.utils;

import com.geometry.types.Circle;
import com.geometry.types.Cylinder;
import com.geometry.types.Point;

import java.util.Arrays;
import java.util.Random;

public class SelectionSortTest {
    private static final Comparable[] arrComparable = new Comparable[9];

    public static void main(String[] args) {
        final Random random = new Random(10);
        for (int i = 0; i < 3; i++) {
            int x = random.nextInt(10, 51);
            int y = random.nextInt(10, 51);

            arrComparable[i] = new Point(x, y);
        }

        for (int i = 3; i < 6; i++) {
            Point point = (Point) arrComparable[i - 3];
            int radius = random.nextInt(10, 31);

            arrComparable[i] = new Circle(point, radius);
        }

        for (int i = 6; i < 9; i++) {
            Circle circle = (Circle) arrComparable[i - 3];
            int height = random.nextInt(10, 61);

            arrComparable[i] = new Cylinder(circle, height);
        }

        System.out.printf("Unsorted array: %s\n", Arrays.toString(arrComparable));
        SelectionSort.sortArray(arrComparable);
        System.out.printf("Sorted array: %s\n", Arrays.toString(arrComparable));
    }
}
