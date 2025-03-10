package pr1;

import shape.Rectangle;

import java.util.Arrays;
import java.util.Scanner;

public class RectangleTest {
    public static void main(String[] args) {
        Rectangle.setColor("blue");

        Rectangle defaultRectangle = new Rectangle();
        defaultRectangle.setHeight(5);
        defaultRectangle.setWidth(4.2);

        Scanner input = new Scanner(System.in);
        System.out.print("Enter width: ");
        double width = input.nextDouble();
        System.out.print("Enter height: ");
        double height = input.nextDouble();

        Rectangle rectangle = new Rectangle(width, height);
        System.out.printf("Dimensions: %.2f x %.2f\nArea: %.2f\nPerimeter: %.0f\n",
                rectangle.getWidth(),
                rectangle.getHeight(),
                rectangle.getArea(),
                rectangle.getPerimeter());
    }
}
