package bg.sofia.uni.fmi.mjt.order.analyzer;

import bg.sofia.uni.fmi.mjt.order.domain.Category;
import bg.sofia.uni.fmi.mjt.order.domain.Order;
import bg.sofia.uni.fmi.mjt.order.domain.PaymentMethod;
import bg.sofia.uni.fmi.mjt.order.domain.Status;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class OrderAnalyzerImplTest {
    @Test
    void testConstructorNullListThrows() {
        assertThrows(IllegalArgumentException.class, () -> new OrderAnalyzerImpl(null),
                     "Should throw IllegalArgumentException when orders list is null");
    }

    // ----------------------------- allOrders -----------------------------

    @Test
    void testAllOrdersReturnsAllOrders() {
        List<Order> input = List.of(
            new OrderBuilder().withId("o-1").build(),
            new OrderBuilder().withId("o-2").withCustomerName("Bob").build()
        );
        var analyzer = new OrderAnalyzerImpl(input);

        assertEquals(input, analyzer.allOrders());
    }

    @Test
    void testAllOrdersReturnDifferentInstanceOfInput() {
        List<Order> input = new ArrayList<>();
        input.add(new OrderBuilder().withId("o-1").build());

        var analyzer = new OrderAnalyzerImpl(input);

        assertNotSame(input, analyzer.allOrders(),
                      "Should return a different instance of the orders list");
    }

    @Test
    void testAllOrdersReturnsUnmodifiableList() {
        List<Order> input = List.of(new OrderBuilder().withId("o-1").build());
        var analyzer = new OrderAnalyzerImpl(input);

        assertThrows(UnsupportedOperationException.class,
                     () -> analyzer.allOrders().add(new OrderBuilder().withId("o-x").build()),
                     "Should not be able to modify returned orders list");
    }

    // ----------------------------- ordersByCustomer -----------------------------

    @Test
    void testOrdersByCustomerShouldThrowWhenCustomerIsNull() {
        var analyzer = new OrderAnalyzerImpl(List.of(new OrderBuilder().build()));
        assertThrows(IllegalArgumentException.class, () -> analyzer.ordersByCustomer(null));
    }

    @Test
    void testOrdersByCustomerShouldThrowWhenCustomerIsBlank() {
        var analyzer = new OrderAnalyzerImpl(List.of(new OrderBuilder().build()));
        assertThrows(IllegalArgumentException.class, () -> analyzer.ordersByCustomer(""));
        assertThrows(IllegalArgumentException.class, () -> analyzer.ordersByCustomer("   "));
    }

    @Test
    void testOrdersByCustomerShouldReturnOnlyOrdersForThatCustomer() {
        Order alice1 = new OrderBuilder().withId("a-1").withCustomerName("Alice").build();
        Order bob1 = new OrderBuilder().withId("b-1").withCustomerName("Bob").build();
        Order alice2 = new OrderBuilder().withId("a-2").withCustomerName("Alice").build();

        var analyzer = new OrderAnalyzerImpl(List.of(alice1, bob1, alice2));
        List<Order> result = analyzer.ordersByCustomer("Alice");

        assertEquals(List.of(alice1, alice2), result,
                     "Should return all orders for the given customer");
    }

    @Test
    void testOrdersByCustomerShouldReturnEmptyListWhenCustomerHasNoOrders() {
        Order alice = new OrderBuilder().withId("a-1").withCustomerName("Alice").build();
        Order bob = new OrderBuilder().withId("b-1").withCustomerName("Bob").build();

        var analyzer = new OrderAnalyzerImpl(List.of(alice, bob));
        List<Order> result = analyzer.ordersByCustomer("Charlie");

        assertNotNull(result);
        assertTrue(result.isEmpty(), "Should return an empty list when customer has no orders");
    }

    @Test
    void testOrdersByCustomerShouldBeCaseSensitive() {
        Order alice = new OrderBuilder().withId("a-1").withCustomerName("Alice").build();
        var analyzer = new OrderAnalyzerImpl(List.of(alice));

        assertTrue(analyzer.ordersByCustomer("alice").isEmpty(),
                   "Customer matching is case-sensitive");
    }

    @Test
    void testOrdersByCustomerShouldReturnUnmodifiableList() {
        var analyzer = new OrderAnalyzerImpl(List.of());
        List<Order> result = analyzer.ordersByCustomer("Alice");

        assertThrows(UnsupportedOperationException.class,
                     () -> result.add(new OrderBuilder().withId("a-2").withCustomerName("Alice").build()),
                     "ordersByCustomer should return an unmodifiable list");
    }

    // ----------------------------- dateWithMostOrders -----------------------------

    @Test
    void testDateWithMostOrdersShouldReturnNullWhenNoOrders() {
        var analyzer = new OrderAnalyzerImpl(List.of());
        assertNull(analyzer.dateWithMostOrders(), "Should return null when there are no orders");
    }

    @Test
    void testDateWithMostOrdersShouldReturnDateWithHighestCount() {
        LocalDate d1 = LocalDate.of(2025, 1, 1);
        LocalDate d2 = LocalDate.of(2025, 1, 2);

        // d1 -> 3 orders, d2 -> 2 orders
        List<Order> orders = List.of(
            new OrderBuilder().withId("o-1").withDate(d1).build(),
            new OrderBuilder().withId("o-2").withDate(d1).build(),
            new OrderBuilder().withId("o-3").withDate(d1).build(),
            new OrderBuilder().withId("o-4").withDate(d2).build(),
            new OrderBuilder().withId("o-5").withDate(d2).build()
        );

        var analyzer = new OrderAnalyzerImpl(orders);
        var result = analyzer.dateWithMostOrders();

        assertNotNull(result);
        assertEquals(d1, result.getKey(), "Should return the date with the most orders");
        assertEquals(3L, result.getValue(), "Should return the correct number of orders for the date");
    }

    @Test
    void testDateWithMostOrdersShouldReturnEarliestDateWhenTieOnCount() {
        LocalDate earlier = LocalDate.of(2025, 1, 1);
        LocalDate later = LocalDate.of(2025, 1, 2);

        // earlier -> 2 orders, later -> 2 orders (tie) => expect earlier
        List<Order> orders = List.of(
            new OrderBuilder().withId("o-1").withDate(later).build(),
            new OrderBuilder().withId("o-2").withDate(earlier).build(),
            new OrderBuilder().withId("o-3").withDate(later).build(),
            new OrderBuilder().withId("o-4").withDate(earlier).build(),
            // extra noise date with fewer orders
            new OrderBuilder().withId("o-5").withDate(LocalDate.of(2025, 1, 3)).build()
        );

        var analyzer = new OrderAnalyzerImpl(orders);
        var result = analyzer.dateWithMostOrders();

        assertNotNull(result);
        assertEquals(earlier, result.getKey(), "On tie, should return the earliest date");
        assertEquals(2L, result.getValue(), "On tie, count should match the tied maximum");
    }

    // ----------------------------- topNMostOrderedProducts -----------------------------

    @Test
    void testTopNMostOrderedProductsShouldThrowWhenNNegative() {
        var analyzer = new OrderAnalyzerImpl(List.of(new OrderBuilder().build()));
        assertThrows(IllegalArgumentException.class, () -> analyzer.topNMostOrderedProducts(-1),
                     "Should throw IllegalArgumentException when n is negative");
    }

    @Test
    void testTopNMostOrderedProductsShouldReturnEmptyListWhenNIsZero() {
        List<Order> orders = List.of(
            new OrderBuilder().withId("o-1").withProduct("A").build(),
            new OrderBuilder().withId("o-2").withProduct("B").build()
        );
        var analyzer = new OrderAnalyzerImpl(orders);

        List<String> result = analyzer.topNMostOrderedProducts(0);

        assertNotNull(result);
        assertTrue(result.isEmpty(), "When n is 0, should return an empty list");
    }

    @Test
    void testTopNMostOrderedProductsShouldReturnEmptyListWhenNoOrders() {
        var analyzer = new OrderAnalyzerImpl(List.of());
        List<String> result = analyzer.topNMostOrderedProducts(5);

        assertNotNull(result);
        assertTrue(result.isEmpty(), "Should return an empty list when there are no orders to chose Top N from");
    }

    @Test
    void testTopNMostOrderedProductsShouldSortByFrequencyDescThenNameAsc() {
        List<Order> orders = List.of(
            new OrderBuilder().withId("o-1").withProduct("B").build(),
            new OrderBuilder().withId("o-2").withProduct("A").build(),
            new OrderBuilder().withId("o-3").withProduct("C").build(),
            new OrderBuilder().withId("o-4").withProduct("B").build(),
            new OrderBuilder().withId("o-5").withProduct("A").build()
        );

        var analyzer = new OrderAnalyzerImpl(orders);

        assertEquals(List.of("A", "B", "C"), analyzer.topNMostOrderedProducts(5),
                     "Should return products sorted by frequency desc then name asc");
    }


    @Test
    void testTopNMostOrderedProductsReturnLimitedProducts() {
        List<Order> orders = List.of(
            new OrderBuilder().withId("o-1").withProduct("B").build(),
            new OrderBuilder().withId("o-2").withProduct("A").build(),
            new OrderBuilder().withId("o-3").withProduct("C").build(),
            new OrderBuilder().withId("o-4").withProduct("B").build(),
            new OrderBuilder().withId("o-5").withProduct("A").build()
        );

        var analyzer = new OrderAnalyzerImpl(orders);

        assertEquals(List.of("A", "B"), analyzer.topNMostOrderedProducts(2),
                     "Should return top 2 products sorted by frequency desc then name asc");
    }

    @Test
    void testTopNMostOrderedProductsShouldReturnAllDistinctWhenNExceedsDistinct() {
        List<Order> orders = List.of(
            new OrderBuilder().withId("o-1").withProduct("X").build(),
            new OrderBuilder().withId("o-2").withProduct("Y").build(),
            new OrderBuilder().withId("o-3").withProduct("X").build()
        );

        var analyzer = new OrderAnalyzerImpl(orders);

        List<String> result = analyzer.topNMostOrderedProducts(10);

        assertEquals(List.of("X", "Y"), result,
                     "If n exceeds the number of distinct products, should return all distinct products");
    }

    @Test
    void testTopNMostOrderedProductsShouldReturnUnmodifiableList() {
        List<Order> orders = List.of(new OrderBuilder().build());
        var analyzer = new OrderAnalyzerImpl(orders);

        List<String> result = analyzer.topNMostOrderedProducts(1);

        assertThrows(UnsupportedOperationException.class,
                     () -> result.add("B"),
                     "Should return an unmodifiable list");
    }

    // ----------------------------- revenueByCategory -----------------------------

    @Test
    void testRevenueByCategoryShouldReturnEmptyMapWhenNoOrders() {
        var analyzer = new OrderAnalyzerImpl(List.of());

        Map<Category, Double> result = analyzer.revenueByCategory();

        assertNotNull(result);
        assertTrue(result.isEmpty(), "Should return an empty map when there are no orders");
    }

    @Test
    void testRevenueByCategoryShouldSumTotalSalesPerCategory() {
        Category c1 = Category.values()[0];
        Category c2 = Category.values().length > 1 ? Category.values()[1] : Category.values()[0];

        // c1 -> 10.25 + 20.50 = 30.75
        // c2 -> 7.00
        List<Order> orders = List.of(
            new OrderBuilder().withId("r-1").withCategory(c1).withTotalSales(10.25).build(),
            new OrderBuilder().withId("r-2").withCategory(c1).withTotalSales(20.50).build(),
            new OrderBuilder().withId("r-3").withCategory(c2).withTotalSales(7.00).build()
        );

        var analyzer = new OrderAnalyzerImpl(orders);

        Map<Category, Double> result = analyzer.revenueByCategory();

        assertNotNull(result);
        assertEquals(2, result.size(), "Should contain revenues for the categories present in the orders");
        assertEquals(30.75, result.get(c1), 0.0001, "Should sum totalSales for the same category");
        assertEquals(7.00, result.get(c2), 0.0001, "Should compute revenue for each category independently");
    }

    @Test
    void testRevenueByCategoryShouldContainOnlyCategoriesPresentInOrders() {
        Category present = Category.values()[0];

        List<Order> orders = List.of(
            new OrderBuilder().withId("r-1").withCategory(present).withTotalSales(1.00).build(),
            new OrderBuilder().withId("r-2").withCategory(present).withTotalSales(2.00).build()
        );

        var analyzer = new OrderAnalyzerImpl(orders);

        Map<Category, Double> result = analyzer.revenueByCategory();

        assertNotNull(result);
        assertEquals(1, result.size(), "Should only include categories that exist in the dataset");
        assertTrue(result.containsKey(present));
        assertEquals(3.00, result.get(present), 1e-9);
    }

    // ----------------------------- suspiciousCustomers -----------------------------

    @Test
    void testSuspiciousCustomersShouldReturnEmptySetWhenNoOrders() {
        var analyzer = new OrderAnalyzerImpl(List.of());

        Set<String> result = analyzer.suspiciousCustomers();

        assertNotNull(result);
        assertTrue(result.isEmpty(), "Should return an empty set when there are no orders");
    }

    @Test
    void testSuspiciousCustomersShouldNotIncludeCustomerWhenSuspiciousOrdersAreLessThan3() {
        List<Order> orders = List.of(
            new OrderBuilder().withId("s-1").withCustomerName("Alice").withStatus(Status.CANCELLED)
                .withTotalSales(99.99).build(),
            new OrderBuilder().withId("s-2").withCustomerName("Alice").withStatus(Status.CANCELLED)
                .withTotalSales(10.00).build(),

            new OrderBuilder().withId("s-3").withCustomerName("Alice").withStatus(Status.CANCELLED)
                .withTotalSales(100.00).build(),
            new OrderBuilder().withId("s-4").withCustomerName("Alice").withStatus(Status.values()[0])
                .withTotalSales(50.00).build()
        );

        var analyzer = new OrderAnalyzerImpl(orders);
        Set<String> result = analyzer.suspiciousCustomers();

        assertFalse(result.contains("Alice"), "Customer with < 3 suspicious orders should not be flagged");
    }

    @Test
    void testSuspiciousCustomersShouldIncludeCustomerWhenSuspiciousOrdersAreExactly3() {
        List<Order> orders = List.of(
            new OrderBuilder().withId("s-1").withCustomerName("Alice").withStatus(Status.CANCELLED)
                .withTotalSales(99.99).build(),
            new OrderBuilder().withId("s-2").withCustomerName("Alice").withStatus(Status.CANCELLED)
                .withTotalSales(10.00).build(),
            new OrderBuilder().withId("s-3").withCustomerName("Alice").withStatus(Status.CANCELLED).withTotalSales(0.01)
                .build(),

            new OrderBuilder().withId("s-4").withCustomerName("Alice").withStatus(Status.CANCELLED)
                .withTotalSales(100.00).build(),
            new OrderBuilder().withId("s-5").withCustomerName("Bob").withStatus(Status.CANCELLED).withTotalSales(99.99)
                .build()
        );

        var analyzer = new OrderAnalyzerImpl(orders);
        Set<String> result = analyzer.suspiciousCustomers();

        assertFalse(result.contains("Alice"), "Customer with exactly 3 suspicious orders should not be flagged");
        assertFalse(result.contains("Bob"), "Bob has only 1 suspicious order and should not be flagged");
    }

    @Test
    void testSuspiciousCustomersShouldIncludeCustomerWhenSuspiciousOrdersAreAtLeast3() {
        List<Order> orders = List.of(
            new OrderBuilder().withId("b-1").withCustomerName("Bob").withStatus(Status.CANCELLED).withTotalSales(99.99)
                .build(),
            new OrderBuilder().withId("b-2").withCustomerName("Bob").withStatus(Status.CANCELLED).withTotalSales(1.00)
                .build(),
            new OrderBuilder().withId("b-3").withCustomerName("Bob").withStatus(Status.CANCELLED).withTotalSales(50.00)
                .build(),
            new OrderBuilder().withId("b-4").withCustomerName("Bob").withStatus(Status.CANCELLED).withTotalSales(0.50)
                .build(),

            new OrderBuilder().withId("c-1").withCustomerName("Charlie").withStatus(Status.CANCELLED)
                .withTotalSales(99.99).build(),
            new OrderBuilder().withId("c-2").withCustomerName("Charlie").withStatus(Status.CANCELLED)
                .withTotalSales(12.00).build(),
            new OrderBuilder().withId("c-3").withCustomerName("Charlie").withStatus(Status.CANCELLED)
                .withTotalSales(0.01).build(),

            new OrderBuilder().withId("d-1").withCustomerName("Diana").withStatus(Status.CANCELLED)
                .withTotalSales(99.99).build(),
            new OrderBuilder().withId("d-2").withCustomerName("Diana").withStatus(Status.CANCELLED)
                .withTotalSales(10.00).build(),

            new OrderBuilder().withId("n-1").withCustomerName("Diana").withStatus(Status.CANCELLED)
                .withTotalSales(100.00).build(),
            new OrderBuilder().withId("n-2").withCustomerName("Bob").withStatus(Status.values()[0])
                .withTotalSales(10.00).build()
        );

        var analyzer = new OrderAnalyzerImpl(orders);
        Set<String> result = analyzer.suspiciousCustomers();

        assertEquals(Set.of("Bob"), result,
                     "Should include customers with > 3 suspicious orders and exclude others");
    }

    @Test
    void testSuspiciousCustomersShouldPassCustomersWithGreaterThan100TotalSalesCanceledPurchases() {
        List<Order> orders = List.of(
            new OrderBuilder().withId("s-1").withCustomerName("Alice").withStatus(Status.CANCELLED).withTotalSales(150)
                .build(),
            new OrderBuilder().withId("s-2").withCustomerName("Alice").withStatus(Status.CANCELLED)
                .withTotalSales(109.00).build(),
            new OrderBuilder().withId("s-3").withCustomerName("Alice").withStatus(Status.CANCELLED).withTotalSales(1000)
                .build(),
            new OrderBuilder().withId("s-4").withCustomerName("Alice").withStatus(Status.CANCELLED)
                .withTotalSales(100.00).build(),
            new OrderBuilder().withId("s-5").withCustomerName("Bob").withStatus(Status.CANCELLED).withTotalSales(99.99)
                .build()
        );

        var analyzer = new OrderAnalyzerImpl(orders);
        Set<String> result = analyzer.suspiciousCustomers();

        assertFalse(result.contains("Alice"),
                    "Should not include customer with more than 3 CANCELLED orders with total sales >= 100");
    }

    // ----------------------------- mostUsedPaymentMethodForCategory -----------------------------

    @Test
    void testMostUsedPaymentMethodForCategoryShouldReturnEmptyMapWhenNoOrders() {
        var analyzer = new OrderAnalyzerImpl(List.of());
        Map<Category, PaymentMethod> result = analyzer.mostUsedPaymentMethodForCategory();

        assertNotNull(result);
        assertTrue(result.isEmpty(), "Should return an empty map when there are no orders");
    }

    @Test
    void testMostUsedPaymentMethodForCategoryShouldReturnMostFrequentPaymentMethodPerCategory() {
        List<Order> orders = List.of(
            new OrderBuilder().withId("p-1").withPaymentMethod(PaymentMethod.GIFT_CARD).build(),
            new OrderBuilder().withId("p-2").withPaymentMethod(PaymentMethod.GIFT_CARD).build(),
            new OrderBuilder().withId("p-3").withPaymentMethod(PaymentMethod.PAYPAL).build()
        );

        var analyzer = new OrderAnalyzerImpl(orders);

        Map<Category, PaymentMethod> result = analyzer.mostUsedPaymentMethodForCategory();

        assertNotNull(result);
        assertEquals(1, result.size(), "Should contain exactly one category entry");
        assertEquals(PaymentMethod.GIFT_CARD, result.get(Category.CLOTHING),
                     "Should return the most frequently used payment method for the category");
    }

    @Test
    void testMostUsedPaymentMethodForCategoryShouldUseAlphabeticalTieBreaker() {
        List<Order> orders = List.of(
            new OrderBuilder().withId("t-1").withPaymentMethod(PaymentMethod.CREDIT_CARD).build(),
            new OrderBuilder().withId("t-2").withPaymentMethod(PaymentMethod.PAYPAL).build(),
            new OrderBuilder().withId("t-3").withPaymentMethod(PaymentMethod.CREDIT_CARD).build(),
            new OrderBuilder().withId("t-4").withPaymentMethod(PaymentMethod.PAYPAL).build()
        );

        var analyzer = new OrderAnalyzerImpl(orders);

        Map<Category, PaymentMethod> result = analyzer.mostUsedPaymentMethodForCategory();

        assertNotNull(result);
        assertEquals(PaymentMethod.CREDIT_CARD, result.get(Category.CLOTHING), // CLOTHING is the test default
                     "On tie, should return the alphabetically-first payment method name");
    }

    @Test
    void testMostUsedPaymentMethodForCategoryShouldComputeIndependentlyPerCategory() {
        List<Order> orders = List.of(
            new OrderBuilder().withId("m-1").withCategory(Category.CLOTHING).withPaymentMethod(PaymentMethod.PAYPAL)
                .build(),
            new OrderBuilder().withId("m-2").withCategory(Category.CLOTHING).withPaymentMethod(PaymentMethod.PAYPAL)
                .build(),
            new OrderBuilder().withId("m-3").withCategory(Category.CLOTHING).withPaymentMethod(PaymentMethod.GIFT_CARD)
                .build(),

            new OrderBuilder().withId("m-4").withCategory(Category.BOOKS).withPaymentMethod(PaymentMethod.GIFT_CARD)
                .build(),
            new OrderBuilder().withId("m-5").withCategory(Category.BOOKS).withPaymentMethod(PaymentMethod.GIFT_CARD)
                .build(),
            new OrderBuilder().withId("m-6").withCategory(Category.BOOKS).withPaymentMethod(PaymentMethod.GIFT_CARD)
                .build(),
            new OrderBuilder().withId("m-7").withCategory(Category.BOOKS).withPaymentMethod(PaymentMethod.PAYPAL)
                .build()
        );

        var analyzer = new OrderAnalyzerImpl(orders);

        Map<Category, PaymentMethod> result = analyzer.mostUsedPaymentMethodForCategory();

        assertNotNull(result);
        assertEquals(2, result.size(), "Should contain an entry for each category present");
        assertEquals(PaymentMethod.PAYPAL, result.get(Category.CLOTHING),
                     "Should select the most used payment method for this category");
        assertEquals(PaymentMethod.GIFT_CARD, result.get(Category.BOOKS),
                     "Should select the most used payment method for this category");
    }

    // ----------------------------- locationWithMostOrders -----------------------------

    @Test
    void testLocationWithMostOrdersShouldReturnNullWhenNoOrders() {
        var analyzer = new OrderAnalyzerImpl(List.of());
        assertNull(analyzer.locationWithMostOrders(), "Should return null when there are no orders");
    }

    @Test
    void testLocationWithMostOrdersShouldReturnLocationWithHighestCount() {
        List<Order> orders = List.of(
            new OrderBuilder().withId("l-1").withCustomerLocation("Sofia").build(),
            new OrderBuilder().withId("l-2").withCustomerLocation("Varna").build(),
            new OrderBuilder().withId("l-3").withCustomerLocation("Sofia").build(),
            new OrderBuilder().withId("l-4").withCustomerLocation("Sofia").build(),
            new OrderBuilder().withId("l-5").withCustomerLocation("Varna").build()
        );

        var analyzer = new OrderAnalyzerImpl(orders);

        assertEquals("Sofia", analyzer.locationWithMostOrders(),
                     "Should return the location with the most orders");
    }

    @Test
    void testLocationWithMostOrdersShouldReturnAlphabeticallySmallestLocationWhenTie() {
        List<Order> orders = List.of(
            new OrderBuilder().withId("t-1").withCustomerLocation("Berlin").build(),
            new OrderBuilder().withId("t-2").withCustomerLocation("Athens").build(),
            new OrderBuilder().withId("t-3").withCustomerLocation("Berlin").build(),
            new OrderBuilder().withId("t-4").withCustomerLocation("Athens").build(),
            new OrderBuilder().withId("t-5").withCustomerLocation("Sofia").build()
        );

        var analyzer = new OrderAnalyzerImpl(orders);

        assertEquals("Athens", analyzer.locationWithMostOrders(),
                     "On tie, should return the alphabetically smallest location");
    }

    @Test
    void testLocationWithMostOrdersShouldWorkWhenAllOrdersSameLocation() {
        List<Order> orders = List.of(
            new OrderBuilder().withId("s-1").withCustomerLocation("Plovdiv").build(),
            new OrderBuilder().withId("s-2").withCustomerLocation("Plovdiv").build(),
            new OrderBuilder().withId("s-3").withCustomerLocation("Plovdiv").build()
        );

        var analyzer = new OrderAnalyzerImpl(orders);

        assertEquals("Plovdiv", analyzer.locationWithMostOrders(),
                     "If all orders are from the same location, should return that location");
    }

    // ----------------------------- groupByCategoryAndStatus -----------------------------

    @Test
    void testGroupByCategoryAndStatusShouldReturnEmptyMapWhenNoOrders() {
        var analyzer = new OrderAnalyzerImpl(List.of());
        Map<Category, Map<Status, Long>> result = analyzer.groupByCategoryAndStatus();

        assertNotNull(result);
        assertTrue(result.isEmpty(), "Should return an empty map when there are no orders");
    }

    @Test
    void testGroupByCategoryAndStatusShouldCountOrdersForEachCategoryAndStatus() {
        // CLOTHING: PENDING -> 2, CANCELLED -> 1
        // BOOKS:    PENDING -> 1, CANCELLED -> 3
        List<Order> orders = List.of(
            // CLOTHING
            new OrderBuilder().withId("g-1").withCategory(Category.CLOTHING).withStatus(Status.PENDING).build(),
            new OrderBuilder().withId("g-2").withCategory(Category.CLOTHING).withStatus(Status.PENDING).build(),
            new OrderBuilder().withId("g-3").withCategory(Category.CLOTHING).withStatus(Status.CANCELLED).build(),

            // BOOKS
            new OrderBuilder().withId("g-4").withCategory(Category.BOOKS).withStatus(Status.PENDING).build(),
            new OrderBuilder().withId("g-5").withCategory(Category.BOOKS).withStatus(Status.CANCELLED).build(),
            new OrderBuilder().withId("g-6").withCategory(Category.BOOKS).withStatus(Status.CANCELLED).build(),
            new OrderBuilder().withId("g-7").withCategory(Category.BOOKS).withStatus(Status.CANCELLED).build()
        );

        var analyzer = new OrderAnalyzerImpl(orders);

        Map<Category, Map<Status, Long>> result = analyzer.groupByCategoryAndStatus();

        assertNotNull(result);
        assertEquals(2, result.size(), "Should contain entries for each category present in the dataset");

        assertTrue(result.containsKey(Category.CLOTHING), "Should contain CLOTHING category");
        assertEquals(2L, result.get(Category.CLOTHING).get(Status.PENDING), "CLOTHING/PENDING count should match");
        assertEquals(1L, result.get(Category.CLOTHING).get(Status.CANCELLED), "CLOTHING/CANCELLED count should match");

        assertTrue(result.containsKey(Category.BOOKS), "Should contain BOOKS category");
        assertEquals(1L, result.get(Category.BOOKS).get(Status.PENDING), "BOOKS/PENDING count should match");
        assertEquals(3L, result.get(Category.BOOKS).get(Status.CANCELLED), "BOOKS/CANCELLED count should match");
    }

    @Test
    void testGroupByCategoryAndStatusShouldNotCreateEntriesForMissingStatuses() {
        // Only PENDING orders for CLOTHING; no CANCELLED orders for CLOTHING
        List<Order> orders = List.of(
            new OrderBuilder().withId("m-1").withCategory(Category.CLOTHING).withStatus(Status.PENDING).build(),
            new OrderBuilder().withId("m-2").withCategory(Category.CLOTHING).withStatus(Status.PENDING).build()
        );

        var analyzer = new OrderAnalyzerImpl(orders);

        Map<Category, Map<Status, Long>> result = analyzer.groupByCategoryAndStatus();

        assertNotNull(result);
        assertEquals(1, result.size());
        assertTrue(result.containsKey(Category.CLOTHING));
        assertEquals(2L, result.get(Category.CLOTHING).get(Status.PENDING));
        assertFalse(result.get(Category.CLOTHING).containsKey(Status.CANCELLED),
                    "Should not contain a status entry when there are no orders with that status");
    }
}
