package lab13b;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class LambdaDemo {
    public static void main(String[] args) {
        Predicate<Salesperson> predicate1 = salesperson ->
                salesperson.getNumSales() > 1200;

//                (Salesperson salesperson) -> {
//            return salesperson.getNumSales() > 1200;
//        };
//                = new Predicate<Salesperson>() {
//            @Override
//            public boolean test(Salesperson salesperson) {
//                return salesperson.getNumSales() > 1200;
//            }
//        };
// да се инициализира
        Predicate<Salesperson> predicate2 =
                salesperson -> salesperson.getNumSales() < 900;
// да се инициализира
        Predicate<Salesperson> predicate = predicate1.or(predicate2);
// да се инициализира
        Consumer<Salesperson> consumer1 = salesperson -> {
            salesperson.addBonus(salesperson.getSalary() * 0.05);
            System.out.println(salesperson);
        };
//                new Consumer<Salesperson>() {
//            @Override
//            public void accept(Salesperson salesperson) {
//                salesperson.addBonus(salesperson.getSalary() * 0.05);
//                System.out.println(salesperson);
//            }
//        };
// да се инициализира
        Consumer<Salesperson> consumer2 = salesperson -> {
            if (predicate1.test(salesperson)) {
                salesperson.addBonus(salesperson.getSalary() * 0.02);
            } else {
                salesperson.addBonus(salesperson.getSalary() * -0.02);
            }
            System.out.println(salesperson);
        };
// да се инициализира
        Comparator<Salesperson> comparator1 = (o1, o2) ->
                Double.compare(o2.getSalary(), o1.getSalary());
// да се инициализира
        Comparator<Salesperson> comparator2 = (o1, o2) -> {
            int salaryCompare = Double.compare(o2.getSalary(), o1.getSalary());
            if (salaryCompare == 0) {
                return o1.getNumSales() - o2.getNumSales();
            }
            return salaryCompare;
        };
// да се инициализира
        Random generator = new Random();
        Salesperson[] salespersons =
                {
                        new Salesperson("John Doe", 2000, 949),
                        new Salesperson("Jane Doe", 3900, 1500),
                        new Salesperson("Jane Doe", 3900, 1400),
                        new Salesperson("Abe Doe",
                                generator.nextInt(200_000, 500_001) / 100.0,
                                generator.nextInt(500, 2000)),
                        new Salesperson("Ann Doe",
                                generator.nextInt(200_000, 500_001) / 100.0,
                                generator.nextInt(500, 2000)),
                        new Salesperson("Dave Doe",
                                generator.nextInt(200_000, 500_001) / 100.0,
                                generator.nextInt(500, 2000)),
                        new Salesperson("Mike Doe",
                                generator.nextInt(200_000, 500_001) / 100.0,
                                generator.nextInt(500, 2000)),
                        new Salesperson("Peter Doe",
                                generator.nextInt(200_000, 500_001) / 100.0,
                                generator.nextInt(500, 2000)),
                        new Salesperson("Carl Doe",
                                generator.nextInt(200_000, 500_001) / 100.0,
                                generator.nextInt(500, 2000)),
                        new Salesperson("Eve Doe",
                                generator.nextInt(200_000, 500_001) / 100.0,
                                generator.nextInt(500, 2000)),
                        new Salesperson("Pan Doe",
                                generator.nextInt(200_000, 500_001) / 100.0,
                                generator.nextInt(500, 2000)),
                        new Salesperson("Matt Doe",
                                generator.nextInt(200_000, 500_001) / 100.0,
                                generator.nextInt(500, 2000)),
// да се добавят още 10 обекти от тип Salesperson
// или да се инициализират с Random стойности
                };
        List<Salesperson> listOfSalespersons = new
                ArrayList<>(Arrays.asList(salespersons));
// обектите на salespersons да се запишат в listOfSalespersons
        for (Salesperson salesperson : salespersons) {
            applyBonus(salesperson, predicate1,
                    consumer1);
            System.out.println(salesperson);
            salesperson.printNumSales(salesperson);
        }
        for (Salesperson salesperson : salespersons) {
            applyBonus(salesperson, predicate2, consumer2);
            System.out.println(salesperson);
        }
        sort(listOfSalespersons, comparator1);
        System.out.println(listOfSalespersons);
        sort(listOfSalespersons, comparator2);
        System.out.println(listOfSalespersons);

        group(listOfSalespersons);

        final Salesperson sumOfTwoSalesmen = listOfSalespersons.get(0).add(listOfSalespersons.get(1), listOfSalespersons.get(2));
        IAdder.printSales(sumOfTwoSalesmen);
    }

    public static void applyBonus(Salesperson salesperson,
                                  Predicate<Salesperson>
                                          predicate,
                                  Consumer<Salesperson>
                                          consumer) {
        if (salesperson == null || predicate == null || consumer == null) return;
        if (predicate.test(salesperson)) {
            consumer.accept(salesperson);
        }
// Напишете if команда, където използвайте predicate
// за да определите дали да изпълните consumer
// Изпълнете consumer, когато условието на if командата е изпълнено
    }

    public static void applyBonus(List<Salesperson>
                                          salespersons,
                                  Predicate<Salesperson>
                                          predicate,
                                  Consumer<Salesperson>
                                          consumer) {
// Напишете if команда, където използвайте predicate
// за да определите дали да изпълните consumer
// Изпълнете consumer, когато условието на if командата е изпълнено
        if (salespersons == null || predicate == null || consumer == null) return;
        for (Salesperson salesperson : salespersons) {
            if (salesperson != null) {
                if (predicate.test(salesperson)) {
                    consumer.accept(salesperson);
                }
            }
        }
    }

    public static void sort(List<Salesperson>
                                    salespersons,
                            Comparator<Salesperson>
                                    comparator) {
// Сортирайте salespersons като използвате comparator
        if (salespersons == null || salespersons.isEmpty() || comparator == null)
            return;
        salespersons.sort(comparator);
    }

    public static void group(List<Salesperson>
                                     salespersons) {
// Групирайте имената на salespersons по първата буква в
// името изведете отделните групи на стандартен изход
        if (salespersons == null || salespersons.isEmpty()) return;
        salespersons.stream()
                .collect(Collectors.groupingBy(
                        salesperson -> salesperson.getName().charAt(0)))
                .forEach((letter, list) -> {
                    System.out.printf("%c:%n", letter);
                    for (Salesperson salesperson : list) {
                        System.out.printf("\t%s%n", salesperson);
                    }
                });

    }
}
