package problem2;

import java.util.Random;

public class SquareMatrixTest {
    public static void main(String[] args) {
        var rng = new Random();

        int N = rng.nextInt(2, 13);
        var input = new int[N][N];
        for (var row : input) {
            for (int i = 0; i < row.length; i++) {
                row[i] = rng.nextInt(2, 13);
            }
        }
        var matrix = new SquareMatrix(N, input);

        System.out.println(matrix);

        System.out.println(matrix.findMaxSum());
        System.out.println();

        matrix.printAll();
    }
}
