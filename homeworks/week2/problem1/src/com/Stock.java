package com;

import static java.lang.Double.valueOf;

public class Stock {
    /**
     * The symbol of the stock
     */
    private String symbol;

    /**
     * The name of the stock
     */
    private String name;

    /**
     * The previous closing price of the stock
     */
    private double previousClosingPrice;

    /**
     * The current price of the stock
     */
    private double currentPrice;

    Stock(String symbol, String name, double previousClosingPrice, double currentPrice) {
        setName(name);
        setSymbol(symbol);
        setPreviousClosingPrice(previousClosingPrice);
        setCurrentPrice(currentPrice);
    }

    Stock(Stock stock) {
        this(stock.getName(), stock.getSymbol(), stock.getPreviousClosingPrice(), stock.getCurrentPrice());
    }

    Stock() {
        this("", "", 0.0, 0.0);
    }

    public double getCurrentPrice() {
        return currentPrice;
    }

    public void setCurrentPrice(double currentPrice) {
        this.currentPrice = currentPrice;
    }

    public double getPreviousClosingPrice() {
        return previousClosingPrice;
    }

    public void setPreviousClosingPrice(double previousClosingPrice) {
        this.previousClosingPrice = previousClosingPrice;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        if (name == null)
            this.name = "";
        else
            this.name = name;
    }

    public String getSymbol() {
        return symbol;
    }

    public void setSymbol(String symbol) {
        if (symbol == null)
            this.symbol = "N/A";
        else
            this.symbol = symbol;
    }

    public double changePercent() {
        return (currentPrice - previousClosingPrice) * 100 / previousClosingPrice;
    }
}
