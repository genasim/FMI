package com.geometry.types;

public class Cylinder extends Circle {
    private int height;

    public Cylinder(int x, int y, int radius, int height) {
        super(x, y, radius);
        setHeight(height);
    }

    public Cylinder(Circle circle, int height) {
        super(circle);
        setHeight(height);
    }

    public int getHeight() {
        return height;
    }

    public void setHeight(int height) {
        this.height = height > 0 ? height : 1;
    }

    @Override
    public int compareTo(Object obj) {
        if (!(obj instanceof Cylinder cylinder)) return 1;

        int pointDiff = super.compareTo(obj);
        if (pointDiff != 0) return pointDiff;

        return height - cylinder.height;
    }

    @Override
    public String toString() {
        return String.format("Cylinder{%s, height=%d}", super.toString(), height);
    }
}
