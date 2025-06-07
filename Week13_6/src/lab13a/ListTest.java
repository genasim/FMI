package lab13a;

import java.util.*;

public class ListTest {
    public static void main(String[] args) {
        //2.2d
        ArrayList<String> aList = new ArrayList<>(
                Arrays.asList("A", "B", "C", "D", "E", "F", "G", "H")
        );
        ArrayList<String> bList = new ArrayList<>(
                Arrays.asList("A", "B", "Z", "K", "E")
        );

        Iterator<String> aIterator = aList.iterator();
        Iterator<String> bIterator;
        String aElement;
        String bElement;

        LinkedHashSet<String> elements = new LinkedHashSet<>();
        while (aIterator.hasNext()) {
            aElement = aIterator.next();
            bIterator = bList.iterator();
            while (bIterator.hasNext()) {
                bElement = bIterator.next();
                if(aElement.equals(bElement)) {
                    elements.add(aElement);
                    break;
                }
            }
        }
        System.out.println("Elements in both lists: " + elements);
        //2.2e
        ArrayList<String> list = new ArrayList<>(
                Arrays.asList("Лили", "Мария", "Георги", "Илия",
                        "Цвета", "Георги")
        );
        ListIterator<String> nameIterator = list.listIterator();
        String name;
        while (nameIterator.hasNext()) {
            name = nameIterator.next();
            if(name.equals("Георги")) {
                nameIterator.add("Симеон");
                break;
            }
        }
        System.out.println("Elements in list: " + list);
    }
}
