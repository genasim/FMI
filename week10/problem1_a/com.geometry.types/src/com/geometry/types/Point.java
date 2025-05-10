package com.geometry.types;

public class Point extends Shape {
    private int x;
    private int y;

    public Point(int x, int y) {
        setX(x);
        setY(y);
    }

    public Point(Point other) {
        this(other.x, other.y);
    }

    public int getX() {
        return x;
    }

    public void setX(int x) {
        this.x = x;
    }

    public int getY() {
        return y;
    }

    public void setY(int y) {
        this.y = y;
    }

    @Override
    public int compareTo(Object obj) {
        if (!(obj instanceof Point other)) return 1;

        int differenceX = x - other.x;
        if (differenceX != 0) return differenceX;

        return y - other.y;
    }

    @Override
    public String toString() {
        return String.format("Point{x=%d, y=%d}", x, y);
    }
}
