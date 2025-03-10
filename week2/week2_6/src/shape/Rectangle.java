package shape;

public class Rectangle
{
    private double width;
    private double height;
    private static String color = "yellow";

    public Rectangle(double width, double height) {
        setWidth(width);
        setHeight(height);
    }

    public Rectangle() {
        this(1,1);
    }

    public static String getColor() {
        return color;
    }

    public static void setColor(String color) {
        Rectangle.color = color;
    }

    public double getWidth() {
        return width;
    }

    public void setWidth(double width) {
        this.width = width > 1 ? width : 1;
    }

    public double getHeight() {
        return height;
    }

    public void setHeight(double height) {
        this.height = height > 1 ? height : 1;
    }

    public double getArea() {
        return width * height;
    }

    public double getPerimeter() {
        return 2 * (width + height);
    }
}
