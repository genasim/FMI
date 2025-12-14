package bg.sofia.uni.fmi.mjt.order.domain;

import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

class OrderTest {

    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("dd-MM-yy");

    private static String validLine(
        String id, LocalDate date, String product, Category category, String price, String quantity, String totalSales,
        String customerName, String customerLocation, PaymentMethod paymentMethod, Status status) {
        return String.join(",", id, date.format(FORMATTER), product, category.name(), price, quantity, totalSales,
                           customerName, customerLocation, paymentMethod.name(), status.name());
    }

    @Test
    void testOfShouldReturnOrderWhenLineIsValid() {
        Category category = Category.values()[0];
        PaymentMethod paymentMethod = PaymentMethod.values()[0];
        Status status = Status.values()[0];

        String line =
            validLine("id-1", LocalDate.of(2025, 12, 14), "Laptop", category, "10.50", "2", "21.00", "John Doe",
                      "Sofia", paymentMethod, status);

        Order o = Order.of(line);

        assertNotNull(o);
        assertEquals("id-1", o.id());
        assertEquals(LocalDate.of(2025, 12, 14), o.date());
        assertEquals("Laptop", o.product());
        assertEquals(category, o.category());
        assertEquals(10.50, o.price(), 0.000_001);
        assertEquals(2, o.quantity());
        assertEquals(21.00, o.totalSales(), 0.000_001);
        assertEquals("John Doe", o.customerName());
        assertEquals("Sofia", o.customerLocation());
        assertEquals(paymentMethod, o.paymentMethod());
        assertEquals(status, o.status());
    }

    @Test
    void testOfShouldTrimCustomerNameAndLocation() {
        Category category = Category.values()[0];
        PaymentMethod paymentMethod = PaymentMethod.values()[0];
        Status status = Status.values()[0];

        String line =
            validLine("id-2", LocalDate.of(2025, 1, 1), "Book", category, " 10.00 ", " 1 ", " 10.00 ", "   Alice   ",
                      "   Varna   ", paymentMethod, status);

        Order o = Order.of(line);

        assertNotNull(o);
        assertEquals("Alice", o.customerName());
        assertEquals("Varna", o.customerLocation());
    }

    @Test
    void testOfShouldReturnNullWhenCategoryIsInvalid() {
        PaymentMethod paymentMethod = PaymentMethod.values()[0];
        Status status = Status.values()[0];

        String line =
            String.join(",", "id-3", LocalDate.of(2025, 12, 14).format(FORMATTER), "Product", "NOT_A_CATEGORY", "10.00",
                        "1", "10.00", "Bob", "Sofia", paymentMethod.name(), status.name());

        assertNull(Order.of(line));
    }

    @Test
    void testOfShouldReturnNullWhenPaymentMethodIsInvalid() {
        Category category = Category.values()[0];
        Status status = Status.values()[0];

        String line =
            String.join(",", "id-4", LocalDate.of(2025, 12, 14).format(FORMATTER), "Product", category.name(), "10.00",
                        "1", "10.00", "Bob", "Sofia", "NOT_A_PAYMENT_METHOD", status.name());

        assertNull(Order.of(line));
    }

    @Test
    void testOfShouldReturnNullWhenStatusIsInvalid() {
        Category category = Category.values()[0];
        PaymentMethod paymentMethod = PaymentMethod.values()[0];

        String line =
            String.join(",", "id-5", LocalDate.of(2025, 12, 14).format(FORMATTER), "Product", category.name(), "10.00",
                        "1", "10.00", "Bob", "Sofia", paymentMethod.name(), "NOT_A_STATUS");

        assertNull(Order.of(line));
    }

    @Test
    void testOfShouldReturnNullWhenPriceIsNotANumber() {
        Category category = Category.values()[0];
        PaymentMethod paymentMethod = PaymentMethod.values()[0];
        Status status = Status.values()[0];

        String line =
            validLine("id-6", LocalDate.of(2025, 12, 14), "Product", category, "abc", "1", "10.00", "Bob", "Sofia",
                      paymentMethod, status);

        assertNull(Order.of(line));
    }

    @Test
    void testOfShouldReturnNullWhenQuantityIsNotAnInteger() {
        Category category = Category.values()[0];
        PaymentMethod paymentMethod = PaymentMethod.values()[0];
        Status status = Status.values()[0];

        String line =
            validLine("id-7", LocalDate.of(2025, 12, 14), "Product", category, "10.00", "1.5", "10.00", "Bob", "Sofia",
                      paymentMethod, status);

        assertNull(Order.of(line));
    }

    @Test
    void testOfShouldReturnNullWhenTotalSalesIsNotANumber() {
        Category category = Category.values()[0];
        PaymentMethod paymentMethod = PaymentMethod.values()[0];
        Status status = Status.values()[0];

        String line =
            validLine("id-8", LocalDate.of(2025, 12, 14), "Product", category, "10.00", "1", "ten", "Bob", "Sofia",
                      paymentMethod, status);

        assertNull(Order.of(line));
    }

    @Test
    void testOfShouldThrowWhenDateFormatIsInvalid() {
        Category category = Category.values()[0];
        PaymentMethod paymentMethod = PaymentMethod.values()[0];
        Status status = Status.values()[0];

        String line = String.join(",", "id-9", "2025-12-14", // invalid for dd-MM-yy
                                  "Product", category.name(), "10.00", "1", "10.00", "Bob", "Sofia",
                                  paymentMethod.name(), status.name());

        assertThrows(DateTimeParseException.class, () -> Order.of(line));
    }

    @Test
    void testOfShouldReturnNullWhenLineHasMissingTokens() {
        String line = "id-10,14-12-25,Product,CATEGORY,10.00,1,10.00,Bob,Sofia,CARD";
        assertNull(Order.of(line));
    }

    @Test
    void testOfShouldThrowWhenLineIsNull() {
        assertThrows(NullPointerException.class, () -> Order.of(null));
    }

    @Test
    void testOfShouldIgnoreExtraTokensAfterStatus() {
        Category category = Category.values()[0];
        PaymentMethod paymentMethod = PaymentMethod.values()[0];
        Status status = Status.values()[0];

        String base =
            validLine("id-11", LocalDate.of(2025, 12, 14), "Product", category, "10.00", "1", "10.00", "Bob", "Sofia",
                      paymentMethod, status);

        Order o = Order.of(base + ",EXTRA,EXTRA2");
        assertNotNull(o);
    }
}
