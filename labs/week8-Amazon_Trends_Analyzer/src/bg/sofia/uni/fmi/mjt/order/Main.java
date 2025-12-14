package bg.sofia.uni.fmi.mjt.order;

import bg.sofia.uni.fmi.mjt.order.analyzer.OrderAnalyzer;
import bg.sofia.uni.fmi.mjt.order.analyzer.OrderAnalyzerImpl;
import bg.sofia.uni.fmi.mjt.order.loader.OrderLoader;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.Reader;
import java.util.Collection;

public class Main {
    static void main(String[] args) throws FileNotFoundException {
//        String filePath = "resources/min_amazon_sales_data 2025.csv";
        String filePath = "resources/amazon_sales_data 2025.csv";

        Reader reader = new FileReader(filePath);

        var orders = OrderLoader.load(reader);
        OrderAnalyzer analyzer = new OrderAnalyzerImpl(orders);

        var analysis = analyzer.dateWithMostOrders();
        System.out.println(analysis);
//        for (var result : analysis) {
//            System.out.println(result);
//        }
    }
}
