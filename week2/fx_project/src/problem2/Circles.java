package problem2;

import javafx.application.Application;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
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

public class Circles extends Application {

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage stage) throws Exception {
        Group group = new Group();
        Scene scene = new Scene(group, 500, 500);

        // TODO  Type code for Java FX drawing objects
        double width = scene.getWidth(); // total width of the scene
        double height = scene.getHeight(); // total width of the scene

        // draw a line from the upper-left to the lower-right
        final Line line1 = new Line(0, 0, width, height);
        line1.setStroke(Color.BLUE);
        line1.setStrokeWidth(2);
        group.getChildren().add(line1);

        // draw a line from the lower-left to the upper-right
        final Line line2 = new Line(0, height, width, 0);
        line2.setStroke(Color.BLUE);
        line2.setStrokeWidth(2);
        group.getChildren().add(line2);

        for (int i = 0; i < 12; i++) {
            drawCircle(group, width / 2, height / 2, 20 * i);
        }

        // end TODO

        stage.setTitle("Drawing scene"); // Update Title as required
        stage.sizeToScene();
        stage.resizableProperty().setValue(Boolean.FALSE);
        stage.setScene(scene);
        stage.show();
    }

    private void drawCircle(Group group, double centerX, double centerY, double radius) {
        final Circle circle = new Circle(centerX, centerY, radius);
        circle.setStroke(Color.BLUE);
        circle.setFill(Color.TRANSPARENT);
        group.getChildren().add(circle);
    }
}
