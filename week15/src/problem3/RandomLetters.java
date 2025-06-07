package problem3;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class RandomLetters {
    public static void main(String[] args) {
        var random = new Random();

        List<Character> characters = IntStream.range(0, 30)
                .mapToObj(i -> (char) ('A' + random.nextInt(26)))
                .toList();

        // a)
        characters.stream()
                .sorted()
                .forEach(letter -> System.out.printf("%c ", letter));
        System.out.println();

        // b)
        characters.stream()
                .sorted(Comparator.reverseOrder())
                .forEach(letter -> System.out.printf("%c ", letter));
        System.out.println();

        // c)
        characters.stream()
                .distinct()
                .sorted()
                .forEach(letter -> System.out.printf("%c ", letter));
        System.out.println();

        // d)
        characters.stream()
                .collect(Collectors.groupingBy(Function.identity(), Collectors.counting()))
                .entrySet().stream()
                .sorted(Map.Entry.<Character, Long>comparingByValue().reversed())
                .forEach(entry -> System.out.printf("%c - %d%n", entry.getKey(), entry.getValue()));
        System.out.println();
    }
}
