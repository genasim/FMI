package problem3;

public class ProbabilityByTwelve {
    public static void main(String[] args) {
        int totalNumbers = 0;
        int divisibleByTwelve = 0;

        int number;
        for (int i = 1; i <= 6; i++) {
            for (int j = 4; j <= 8; j++) {
                for (int k = 3; k <= 7; k++) {
                    for (int l = 1; l <= 9; l++) {
                        for (int m = 1; m <= 8; m++) {
                            totalNumbers++;
                            number = (i * 10000) + (j * 1000) + (k * 100) + (l * 10) + m;
                            if (number % 12 == 0) divisibleByTwelve++;
                        }
                    }
                }
            }
        }

        double probabilityByTwelve = (double) divisibleByTwelve / totalNumbers;
        System.out.printf("Total numbers with desired properties: %d\n", totalNumbers);
        System.out.printf("Total numbers divisable by twelve: %d\n", divisibleByTwelve);
        System.out.printf("Probability by twelve: %.2f\n", probabilityByTwelve);
    }
}
