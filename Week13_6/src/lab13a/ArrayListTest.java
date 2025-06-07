package lab13a;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

public class ArrayListTest {
    //a)
    public static <E extends Comparable<E>> E max(E[][]
                                                          list) {
        if (list == null || list.length == 0) return null;
        ArrayList<E> elements = new ArrayList<>();
        for (int i = 0; i < list.length; i++) {
            elements.addAll(Arrays.asList(list[i]));
        }
        return Collections.max(elements);
    }

    //b)
    public static <E> void shuffle(ArrayList<E> list) {
        if (list == null || list.isEmpty()) return;
        Collections.shuffle(list);
    }

    //c)
    public static <E extends Comparable<E>> E
    min(ArrayList<E> list) {
        if (list == null || list.isEmpty()) return null;
        return Collections.min(list);
    }

    //d)
    public static <E> ArrayList<E>
    removeDuplicates(ArrayList<E> list) {
        if (list == null || list.isEmpty()) return null;
        ArrayList<E> noDuplicates = new ArrayList<>();
        for (int i = 0; i < list.size(); i++) {
            if (Collections.frequency(noDuplicates, list.get(i)) == 0) {
                noDuplicates.add(list.get(i));
            }
        }
        return noDuplicates;
    }

    public static void main(String[] args) {
        String[][] matrix = new String[][]{
                {"a", "b", "c"},
                {"d", "e", "f"},
                {"g", "h", "i"}
        };
        System.out.println("Max in array: " + max(matrix));

        ArrayList<Integer> list = new ArrayList<>();
        for (int i = 0; i < 20; i++) {
            list.add(i % 6);
        }
        shuffle(list);
        System.out.println("Shuffled: " + list);
        System.out.println("Min in list: " + min(list));
        System.out.println("No duplicates in list: " +
                removeDuplicates(list));
    }
}
