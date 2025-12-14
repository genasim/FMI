package bg.sofia.uni.fmi.mjt.order.analyzer;

import bg.sofia.uni.fmi.mjt.order.domain.Category;
import bg.sofia.uni.fmi.mjt.order.domain.Order;
import bg.sofia.uni.fmi.mjt.order.domain.PaymentMethod;
import bg.sofia.uni.fmi.mjt.order.domain.Status;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class OrderAnalyzerImpl implements OrderAnalyzer {
    private final List<Order> orders;

    public OrderAnalyzerImpl(List<Order> orders) {
        if (orders == null) {
            throw new IllegalArgumentException("Cannot instantiate analyzer with null orders list");
        }
        this.orders = new ArrayList<>(orders);
    }

    @Override
    public List<Order> allOrders() {
        return List.copyOf(orders);
    }

    @Override
    public List<Order> ordersByCustomer(String customer) {
        if (customer == null || customer.isBlank()) {
            throw new IllegalArgumentException("Customer cannot be null or blank");
        }
        return orders.stream().filter(order -> order.customerName().equals(customer)).toList();
    }

    @Override
    public Map.Entry<LocalDate, Long> dateWithMostOrders() {
        if (orders.isEmpty()) {
            return null;
        }

        return orders.stream()
            .collect(Collectors.groupingBy(Order::date, Collectors.counting()))
            .entrySet().stream()
            .max(Map.Entry.<LocalDate, Long>comparingByValue()
                     .thenComparing(Map.Entry::getKey, Comparator.reverseOrder()))
            .orElse(null);
    }

    @Override
    public List<String> topNMostOrderedProducts(int n) {
        if (n < 0) {
            throw new IllegalArgumentException("Number of products cannot be negative");
        }

        if (orders.isEmpty() || n == 0) {
            return Collections.emptyList();
        }

        var mostOrderedProducts = orders.stream()
            .collect(Collectors.groupingBy(Order::product, Collectors.counting()));
        return mostOrderedProducts.entrySet().stream()
            .sorted(Map.Entry.<String, Long>comparingByValue(Comparator.reverseOrder())
                        .thenComparing(Map.Entry.comparingByKey())
            )
            .limit(n)
            .map(Map.Entry::getKey)
            .toList();
    }

    @Override
    public Map<Category, Double> revenueByCategory() {
        return orders.stream().collect(
            Collectors.groupingBy(Order::category, Collectors.summingDouble(Order::totalSales))
        );
    }

    @Override
    public Set<String> suspiciousCustomers() {
        if (orders.isEmpty()) {
            return Collections.emptySet();
        }

        final int maxCancelledOrders = 3;
        final double minTotalSales = 100.0;

        return orders.stream()
            .filter(order -> order.status() == Status.CANCELLED && order.totalSales() < minTotalSales)
            .collect(Collectors.groupingBy(Order::customerName, Collectors.counting()))
            .entrySet().stream()
            .filter(entry -> entry.getValue() > maxCancelledOrders)
            .map(Map.Entry::getKey)
            .collect(Collectors.toSet());
    }

    @Override
    public Map<Category, PaymentMethod> mostUsedPaymentMethodForCategory() {
        if (orders.isEmpty()) {
            return Collections.emptyMap();
        }

        var byCountThenName = Comparator.<Map.Entry<PaymentMethod, Long>>comparingLong(Map.Entry::getValue)
            .thenComparing(e -> e.getKey().name(), Comparator.reverseOrder());

        return orders.stream()
            .collect(Collectors.groupingBy(
                Order::category,
                Collectors.collectingAndThen(
                    Collectors.groupingBy(Order::paymentMethod, Collectors.counting()),
                    paymentCounts -> paymentCounts.entrySet().stream()
                        .max(byCountThenName)
                        .orElseThrow()
                        .getKey()
                )
            ));
    }

    @Override
    public String locationWithMostOrders() {
        if (orders.isEmpty()) {
            return null;
        }

        return orders.stream()
            .collect(Collectors.groupingBy(Order::customerLocation, Collectors.counting()))
            .entrySet().stream()
            .min(Map.Entry.<String, Long>comparingByValue().reversed()
                     .thenComparing(Map.Entry.comparingByKey()))
            .map(Map.Entry::getKey)
            .orElse(null);
    }

    @Override
    public Map<Category, Map<Status, Long>> groupByCategoryAndStatus() {
        if (orders.isEmpty()) {
            return Collections.emptyMap();
        }

        return orders.stream()
            .collect(Collectors.groupingBy(
                Order::category,
                Collectors.collectingAndThen(Collectors.groupingBy(Order::status, Collectors.counting()), Map::copyOf)
            )).entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }
}
