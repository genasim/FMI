package week14;

import java.util.Arrays;
import java.util.function.*;

public class LambdaDemo {
    public static void method(Function<Double, Double> function) {
        System.out.println(function.apply(20.0));
    }

    public static void main(String[] args) {
        // a.a
        Function<Integer, String> function = number -> String.valueOf(number);
        System.out.println(function.apply(10));

        // a.b
        BiPredicate<Double, Double> predicate = (x, y) -> x % y == 0;
        System.out.println(predicate.test(6d, 3.));

        // a.c
        BiConsumer<String, String> consumer = (str1, str2) ->
                System.out.format("%s %s\n", str1, str2);
        consumer.accept("hello", "world");

        // a.d
        Supplier<int[]> supplier = () -> new int[]{1, 2, 3, 4, 20, 5, 6};
        System.out.println(Arrays.toString(supplier.get()));

        // a.e
        IntFunction<double[]> intFunction = size -> new double[size];
        System.out.println(Arrays.toString(intFunction.apply(10)));


        // b.a
        Function<Double, Double> func = Math::cos;

        // b.b
        Consumer<String> task = System.out::println;

        // b.c
        Runnable runnable = System.out::println;

        // b.d
        Predicate<String> isEqual = ""::equals;


        // c.a
        method(number -> Math.pow(number, 4));
        // c.b
        method(Math::abs);
        // c.c
        method(new LambdaDemo()::foo);
    }

    private double foo(double x) {
        return x * x;
    }
}
