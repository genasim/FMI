package geometry;

import javafx.scene.Group;
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;

public class Line extends Point {
    private Point ePoint; //ending point

    public Line() {
        this(new Point(), new Point(new int[]{1, 1}));
    }

    public Line(Point sPoint, Point ePoint) {
        setePoint(ePoint);
        setsPoint(sPoint);
    }

    public Line(Line l) {
        this(l.getsPoint(), l.getePoint());
    }

    public Point getsPoint() {
        return new Point(getPoint());
    }

    public void setsPoint(Point start) {
        setPoint(start);
    }

    public Point getePoint() {
        return new Point(ePoint);
    }

    public void setePoint(Point ePoint) {
        if (ePoint != null) {
            this.ePoint = new Point(ePoint);
        } else {
            this.ePoint = new Point(new int[]{1, 1});
        }
    }

    public void draw(Pane pane) {
//        javafx.scene.shape.Line line = new javafx.scene.shape.Line(
//                getsPoint().getCoords()[0], getsPoint().getCoords()[1],
//                ePoint.getCoords()[0], ePoint.getCoords()[1]
//        );
//        pane.getChildren().add(line);
        int startX = getsPoint().getCoords()[0];
        int startY = getsPoint().getCoords()[1];
        int endX = ePoint.getCoords()[0];
        int endY = ePoint.getCoords()[1];

        final javafx.scene.shape.Line line = new javafx.scene.shape.Line(startX, startY, endX, endY);
        line.setStroke(Color.BLACK);
        pane.getChildren().add(line);
    }

    public double measure() {
        int startX = getsPoint().getCoords()[0];
        int startY = getsPoint().getCoords()[1];
        int endX = ePoint.getCoords()[0];
        int endY = ePoint.getCoords()[1];

        return Math.sqrt(Math.pow(endX - startX, 2) + Math.pow(endY - startY, 2));
    }

    @Override
    public String toString() {
        return String.format("Starting point coords: %s," +
                "Ending point coords: %s", getPoint(), ePoint);
    }
}
