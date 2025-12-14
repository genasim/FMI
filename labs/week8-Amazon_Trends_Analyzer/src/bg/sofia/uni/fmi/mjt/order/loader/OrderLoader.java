package bg.sofia.uni.fmi.mjt.order.loader;

import bg.sofia.uni.fmi.mjt.order.domain.Order;

import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class OrderLoader {
    private OrderLoader() {
    }

    /**
     * Returns a list of orders read from the source Reader.
     *
     * @param reader the Reader with orders
     * @throws IllegalArgumentException if the reader is null
     */
    public static List<Order> load(Reader reader) {
        if (reader == null) {
            throw new IllegalArgumentException("Reader cannot be null");
        }

        List<String> rawOrders;
        try {
            rawOrders = new ArrayList<>(reader.readAllLines().stream().skip(1).toList());
            reader.close();
        } catch (IOException e) {
//            throw new CouldNotReadDatasetLineException("Could not read all lines of csv dataset", e);
            System.err.println("Could not read all lines of csv dataset; returning empty list");
            return List.of();
        }

        return rawOrders.stream().map(Order::of).filter(Objects::nonNull).toList();
    }
}