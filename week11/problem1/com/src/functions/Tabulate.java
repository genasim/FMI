package functions;

import services.Computable;

public class Tabulate {
    private Computable computable;

    Tabulate(Computable computable) {
        setComputable(computable);
    }

    public Computable getComputable() {
        return computable;
    }

    public void setComputable(Computable computable) {
        if (computable == null) {
            this.computable = new Computable() {
                @Override
                public double function(double x) {
                    return 0;
                }
            };
            return;
        }
        this.computable = computable;
    }

    public void tabulate(double a, double b, int steps) {
        tabulateFunction(a, b, steps, computable);
    }

    public static void tabulateFunction(double a, double b, int steps, Computable computable) throws IllegalArgumentException {
        if (a >= b)
            throw new IllegalArgumentException("a must be smaller than b");
        if (steps <= 0)
            throw new IllegalArgumentException("steps must be greater than 0");
        if (computable == null)
            throw new IllegalArgumentException("computable callback is null");

        System.out.printf("%-20s%-20s%n", "X", "F(X)");
        double step = (b - a) / steps;
        for (double x = a; x <= b; x += step) {
            System.out.printf("%-20f%-20f%n", x, computable.function(x));
        }
    }
}
