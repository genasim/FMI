package bg.sofia.uni.fmi.mjt.order.analyzer;

import bg.sofia.uni.fmi.mjt.order.domain.Category;
import bg.sofia.uni.fmi.mjt.order.domain.Order;
import bg.sofia.uni.fmi.mjt.order.domain.PaymentMethod;
import bg.sofia.uni.fmi.mjt.order.domain.Status;

import java.time.LocalDate;


/**
 * Test-only builder with sane defaults for creating valid {@link Order} instances.
 * Kept in the test source set to avoid polluting production code.
 */
class OrderBuilder {
    private String id = "id-1";
    private LocalDate date = LocalDate.of(2025, 12, 14);
    private String product = "Product";
    private Category category = Category.CLOTHING;
    private double price = 10.00;
    private int quantity = 1;
    private double totalSales = 10.00;
    private String customerName = "Alice";
    private String customerLocation = "Sofia";
    private PaymentMethod paymentMethod = PaymentMethod.AMAZON_PAY;
    private Status status = Status.PENDING;

    OrderBuilder withId(String id) {
        this.id = id;
        return this;
    }

    OrderBuilder withDate(LocalDate date) {
        this.date = date;
        return this;
    }

    OrderBuilder withProduct(String product) {
        this.product = product;
        return this;
    }

    OrderBuilder withCategory(Category category) {
        this.category = category;
        return this;
    }

    OrderBuilder withPrice(double price) {
        this.price = price;
        return this;
    }

    OrderBuilder withQuantity(int quantity) {
        this.quantity = quantity;
        return this;
    }

    OrderBuilder withTotalSales(double totalSales) {
        this.totalSales = totalSales;
        return this;
    }

    OrderBuilder withCustomerName(String customerName) {
        this.customerName = customerName;
        return this;
    }

    OrderBuilder withCustomerLocation(String customerLocation) {
        this.customerLocation = customerLocation;
        return this;
    }

    OrderBuilder withPaymentMethod(PaymentMethod paymentMethod) {
        this.paymentMethod = paymentMethod;
        return this;
    }

    OrderBuilder withStatus(Status status) {
        this.status = status;
        return this;
    }

    Order build() {
        return new Order(id, date, product, category, price, quantity, totalSales, customerName, customerLocation,
                         paymentMethod, status);
    }
}
