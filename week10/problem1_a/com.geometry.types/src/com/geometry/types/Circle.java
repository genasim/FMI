package com.geometry.types;

public class Circle extends Point {
    private int radius;

    public Circle(int x, int y, int radius) {
        super(x, y);
        setRadius(radius);
    }

    public Circle(Point point, int radius) {
        super(point);
        setRadius(radius);
    }

    public Circle(Circle other) {
        this(other.getX(), other.getY(), other.getRadius());
    }

    public int getRadius() {
        return radius;
    }

    public void setRadius(int radius) {
        this.radius = radius > 0 ? radius : 1;
    }

    @Override
    public int compareTo(Object obj) {
        if (!(obj instanceof Circle circle)) return 1;

        int pointDiff = super.compareTo(obj);
        if (pointDiff != 0) return pointDiff;

        return radius - circle.radius;
    }

    @Override
    public String toString() {
        return String.format("Circle{%s, radius=%d}", super.toString(), radius);
    }
}
