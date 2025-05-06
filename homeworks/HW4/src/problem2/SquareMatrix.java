package problem2;

import java.util.*;

public class SquareMatrix {
    private final int dimension;
    private int[][] dataArray;

    SquareMatrix() {
        this(2, new int[2][2]);
    }

    SquareMatrix(int dimension, int[][] dataArray) {
        this.dimension = dimension;
        setDataArray(dataArray);
    }

    SquareMatrix(SquareMatrix other) {
        this(other.dimension, other.dataArray);
    }

    public int getDimension() {
        return dimension;
    }

    public int[][] getDataArray() {
        return copyOfMatrix(dataArray);
    }

    public void setDataArray(int[][] dataArray) {
        if (dataArray.length != dimension ||
                Arrays.stream(dataArray).anyMatch(row -> row.length != dimension)) {
            throw new IllegalArgumentException("Input matrix must be square and of dimension " + dimension);
        }
        this.dataArray = copyOfMatrix(dataArray);
    }

    @Override
    public String toString() {
        var sb = new StringBuilder();
        for (var row : dataArray) {
            for (var item : row) {
                sb.append(item);
                sb.append(" ");
            }
            sb.append("\n");
        }
        return sb.toString();
    }

    public int sum() {
        return Arrays.stream(dataArray).flatMapToInt(Arrays::stream).sum();
    }

    public int findMaxSum() {
        int max = Integer.MIN_VALUE;

        for (int i = 0; i < dimension - 1; i++) {
            for (int j = 0; j < dimension - 1; j++) {
                var submatrix = new SquareMatrix(2, new int[][]{
                        {dataArray[i][j], dataArray[i][j + 1]},
                        {dataArray[i + 1][j], dataArray[i + 1][j + 1]}
                });

                max = Math.max(max, submatrix.sum());
            }
        }

        return max;
    }

    public void printAll() {
        int maxSum = Integer.MIN_VALUE;
        List<int[]> topLeftIndices = new LinkedList<>();

        for (int i = 0; i < dimension - 1; i++) {
            for (int j = 0; j < dimension - 1; j++) {
                var submatrix = new SquareMatrix(2, new int[][]{
                        {dataArray[i][j], dataArray[i][j + 1]},
                        {dataArray[i + 1][j], dataArray[i + 1][j + 1]}
                });

                int sum = submatrix.sum();
                if (sum > maxSum) {
                    maxSum = sum;
                    topLeftIndices.clear();
                }

                if (sum == maxSum) {
                    topLeftIndices.add(new int[]{i, j});
                }
            }
        }

        assert !topLeftIndices.isEmpty();
        System.out.println("Maximum 2x2 submatrix sum: " + maxSum);
        System.out.println("Matching submatrices and their top-left indices:");

        for (int[] coords : topLeftIndices) {
            System.out.println("[" + coords[0] + ", " + coords[1] + "]");
        }
    }

    private int[][] copyOfMatrix(int[][] matrix) {
        int[][] copyDataArray = new int[matrix.length][matrix[0].length];
        for (int i = 0; i < copyDataArray.length; i++)
            System.arraycopy(matrix[i], 0, copyDataArray[i], 0, copyDataArray[i].length);
        return copyDataArray;
    }
}
