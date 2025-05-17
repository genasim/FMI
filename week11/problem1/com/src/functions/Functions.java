package functions;

import services.Computable;

public class Functions {
    public static Computable sin() { return new SinFunction(); }
    public static Computable exp() { return new ExpFunction(); }

    private static class SinFunction implements Computable {
        @Override
        public double function(double x) {
            return Math.sin(x);
        }
    }

    private static class ExpFunction implements Computable {
        @Override
        public double function(double x) {
            return Math.exp(x);
        }
    }
}
