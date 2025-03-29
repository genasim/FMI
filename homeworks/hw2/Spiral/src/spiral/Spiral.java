package spiral;

import javafx.application.Application;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.paint.Color;
import javafx.scene.shape.Line;
import javafx.stage.Stage;
// Abbreviaton key:       fx-drawing-main
// Template description: JavaFX App class for drawing
// Variables: CLASS_NAME must be assigned clipboard() expression
// 1. Create a Java class
// 2. Copy the class name in the Clipboard (^C)
// 3, Overwrite all  the class contents by running this Live template
// 4. Right-click the class name (should be the same as in the originally created class)
// 5. Select Show Content actions and execute Set package name to ...<your package name>

public class Spiral extends Application {

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage stage) throws Exception {
        final double DIMENSION = 500;
        final Group group = new Group();
        final Scene scene = new Scene(group, DIMENSION, DIMENSION);

        final double center = DIMENSION / 2;
        double x = center, y = center;

        final int SEGMENTS = 25;
        int step = 30;
        int direction;

        for (int i = 0; i < SEGMENTS; i++) {
            double startX = x;
            double startY = y;

            direction = i % 4;
            switch (direction) {
                case 0:
                    y += step;
                    break;
                case 1:
                    x -= step;
                    break;
                case 2:
                    y -= step;
                    break;
                case 3:
                    x += step;
                    break;
            }

            drawSpiralLine(group, startX, startY, x, y);
            if (i % 2 == 1) step += 30;
        }

        stage.setTitle("Square spiral");
        stage.sizeToScene();
        stage.resizableProperty().setValue(Boolean.FALSE);
        stage.setScene(scene);
        stage.show();
    }

    private void drawSpiralLine(Group group, double startX, double startY, double endX, double endY) {
        Line line = new Line(startX, startY, endX, endY);
        line.setStroke(Color.RED);
        line.setStrokeWidth(3);
        group.getChildren().add(line);
    }
}
