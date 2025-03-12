package com;

public class StockTest {
    public static void main(String[] args) {
        testDefaultStock();
        testCopyStock();
    }

    private static void testDefaultStock() {
        Stock defaultStock = new Stock();
        printStock(defaultStock);
    }

    private static void testCopyStock() {
        Stock original = new Stock("ABC", "Alphabet", 20, 30);
        Stock copy = new Stock(original);

        original.setName("Google");
        original.setCurrentPrice(40);

        printStock(original);
        printStock(copy);
    }

    private static void printStock(Stock stock) {
        System.out.printf("Stock infromation: " +
                        "\nSymbol: %s " +
                        "\nName: %s " +
                        "\nPrevious price: %.03f " +
                        "\nCurrent Price: %.03f \n",
                stock.getSymbol(),
                stock.getName(),
                stock.getPreviousClosingPrice(),
                stock.getCurrentPrice());
    }
}
