package bg.sofia.uni.fmi.mjt.order.domain;

import bg.sofia.uni.fmi.mjt.order.loader.EnumParser;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public record Order(String id, LocalDate date, String product, Category category, double price, int quantity,
                    double totalSales, String customerName, String customerLocation, PaymentMethod paymentMethod,
                    Status status) {
    private enum TokenIndex {
        ORDER_ID(0), DATE(1), PRODUCT(2), CATEGORY(3), PRICE(4), QUANTITY(5), TOTAL_SALES(6), CUSTOMER_NAME(7),
        CUSTOMER_LOCATION(8), PAYMENT_METHOD(9), STATUS(10);

        private final int index;

        TokenIndex(int index) {
            this.index = index;
        }
    }

    public static Order of(String line) {
        String[] tokens = line.split(",");

        try {
            String orderId = getTokenAt(TokenIndex.ORDER_ID, tokens);

            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd-MM-yy");
            LocalDate date = LocalDate.parse(getTokenAt(TokenIndex.DATE, tokens), formatter);

            String product = getTokenAt(TokenIndex.PRODUCT, tokens);
            Category category = EnumParser.parse(Category.class, getTokenAt(TokenIndex.CATEGORY, tokens));
            double price = Double.parseDouble(getTokenAt(TokenIndex.PRICE, tokens));
            int quantity = Integer.parseInt(getTokenAt(TokenIndex.QUANTITY, tokens));
            double totalSales = Double.parseDouble(getTokenAt(TokenIndex.TOTAL_SALES, tokens));
            String customerName = getTokenAt(TokenIndex.CUSTOMER_NAME, tokens);
            String customerLocation = getTokenAt(TokenIndex.CUSTOMER_LOCATION, tokens);
            PaymentMethod paymentMethod =
                EnumParser.parse(PaymentMethod.class, getTokenAt(TokenIndex.PAYMENT_METHOD, tokens));
            Status status = EnumParser.parse(Status.class, getTokenAt(TokenIndex.STATUS, tokens));

            return new Order(orderId, date, product, category, price, quantity, totalSales, customerName,
                             customerLocation, paymentMethod, status);
        } catch (IllegalArgumentException | ArrayIndexOutOfBoundsException _) {
            return null;
        }
    }

    private static String getTokenAt(TokenIndex index, String[] tokens) {
        return tokens[index.index].trim();
    }

    @Override
    public boolean equals(Object o) {
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        Order order = (Order) o;
        return id().equals(order.id());
    }

    @Override
    public int hashCode() {
        return id().hashCode();
    }
}
