package geometry;

import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;

public class Rectangle extends Point {
    private Point lPoint;

    public Rectangle() {
        this(new Point(), new Point(new int[]{1, 1}));
    }

    public Rectangle(Point uPoint, Point lPoint) {
        setuPoint(uPoint);

    }

    public Rectangle(Rectangle r) {
        this(r.getuPoint(), r.lPoint);
    }

    public Point getuPoint() {
        return getPoint();
    }

    public void setuPoint(Point uPoint) {
        setPoint(uPoint);
    }

    public Point getlPoint() {
        return new Point(lPoint);
    }

    public void setlPoint(Point lPoint) {
        this.lPoint = lPoint != null ? new Point(lPoint) : new Point(new int[]{1, 1});
    }

    public double getWidth() {
        double upperX = getuPoint().getCoords()[0];
        double lowerX = getlPoint().getCoords()[0];

        return Math.abs(upperX - lowerX);
    }

    public double getHeight() {
        double upperY = getuPoint().getCoords()[1];
        double lowerY = getlPoint().getCoords()[1];

        return Math.abs(upperY - lowerY);
    }

    public void draw(Pane pane) {
        javafx.scene.shape.Rectangle rectangle =
                new javafx.scene.shape.Rectangle(
                        getuPoint().getCoords()[0], getuPoint().getCoords()[1],
                        getWidth(), getHeight()
                );
        rectangle.setFill(Color.TRANSPARENT);
        rectangle.setStroke(Color.BLACK);
        pane.getChildren().add(rectangle);
    }

    @Override
    public String toString() {
        return String.format("Rectangle{ uPoint: %s, lPoint: %s }",
                getuPoint(), lPoint);
    }
}
