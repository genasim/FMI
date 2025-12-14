package bg.sofia.uni.fmi.mjt.order.loader;

import bg.sofia.uni.fmi.mjt.order.domain.Order;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class OrderLoaderTest {
    @Test
    void testLoadNullReaderThrows() {
        assertThrows(IllegalArgumentException.class, () -> OrderLoader.load(null),
                     "Should throw IllegalArgumentException when reader is null");
    }

    @Test
    void testLoadErrorWhileReadingReturnsEmptyList() throws IOException {
        Reader reader = new StringReader("Invalid CSV");
        reader.close();

        assertEquals(List.of(), OrderLoader.load(reader));
    }

    @Test
    void testLoadEmptyFileReturnsEmptyList() {
        assertEquals(List.of(), OrderLoader.load(new StringReader("")));
    }

    @Test
    void testLoadCsvHeaderOnlyReturnsEmptyList() {
        Reader reader = new StringReader(
            """
                Customer,Order Date,Order Status,Order Total,Category,Location
                """);
        assertEquals(List.of(), OrderLoader.load(reader));
    }

    @Test
    void testLoadCsvFileReturnsOrders() {
        Reader reader = new StringReader(
            """
                 Order ID,Date,Product,Category,Price,Quantity,Total Sales,Customer Name,Customer Location,Payment Method,Status
                 ORD0002,20-03-25,Headphones,Electronics,100,4,400,Emily Johnson,San Francisco,Debit Card,Pending
                 ORD0003,15-02-25,Running Shoes,Footwear,60,2,120,John Doe,Denver,Amazon Pay,Cancelled
                """);
        List<Order> orders = OrderLoader.load(reader);

        assertEquals(2, orders.size());
    }

    @Test
    void testLoadCsvWithInvalidRowsFiltersThemOut() {
        Reader reader = new StringReader(
            """
                 Order ID,Date,Product,Category,Price,Quantity,Total Sales,Customer Name,Customer Location,Payment Method,Status
                 ORD0002,20-03-25,Headphones,Electronics,100,4,400,Emily Johnson,San Francisco,,Pending
                 ORD0003,15-02-25,Running Shoes,Footwear,60,2,120,John Doe,Denver,Amazon Pay,Cancelled
                """);
        List<Order> orders = OrderLoader.load(reader);

        assertEquals(1, orders.size());
    }

    @Test
    void testLoadReturnedListWithOrdersIsImmutable() {
        Reader reader = new StringReader(
            """
                 Order ID,Date,Product,Category,Price,Quantity,Total Sales,Customer Name,Customer Location,Payment Method,Status
                 ORD0002,20-03-25,Headphones,Electronics,100,4,400,Emily Johnson,San Francisco,Debit Card,Pending
                 ORD0003,15-02-25,Running Shoes,Footwear,60,2,120,John Doe,Denver,Amazon Pay,Cancelled
                """);
        List<Order> orders = OrderLoader.load(reader);

        assertThrows(UnsupportedOperationException.class, () -> orders.add(null),
                     "Returned list should be immutable");
    }
}
