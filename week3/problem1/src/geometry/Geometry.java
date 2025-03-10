package geometry;

import javafx.application.Application;
import javafx.application.Platform;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.control.TextInputDialog;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import javafx.scene.shape.Line;
import javafx.scene.text.Text;
import javafx.stage.Stage;

import java.util.Optional;
// Abbreviaton key:       fx-drawing-main
// Template description: JavaFX App class for drawing
// Variables: CLASS_NAME must be assigned clipboard() expression
// 1. Create a Java class
// 2. Copy the class name in the Clipboard (^C)
// 3, Overwrite all  the class contents by running this Live template
// 4. Right-click the class name (should be the same as in the originally created class)
// 5. Select Show Content actions and execute Set package name to ...<your package name>

public class Geometry extends Application {
    double sceneWidth;
    double sceneHeight;

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage stage) throws Exception {
        Group group = new Group();
        Scene scene = new Scene(group, 300, 300);

        sceneWidth = scene.getWidth();
        sceneHeight = scene.getHeight();

        final double radius = Math.min(sceneHeight, sceneWidth) / 3;
        double centerX = sceneWidth / 2;
        double centerY = sceneHeight / 2;

        Circle circle = new Circle(centerX, centerY, radius);
        circle.setFill(null);
        circle.setStroke(Color.BLUE);

        final double lineX = getLineX();
        Line abLine = new Line(lineX, 0, lineX, sceneHeight);
        abLine.setStroke(Color.RED);

        group.getChildren().addAll(circle, abLine);

        final double chSquared = radius * radius - (centerX - lineX) * (centerX - lineX);

        if (chSquared < 0) {
            displayAlert("Warning", "No intersection found", "", Alert.AlertType.WARNING);
            Platform.exit();
            return;
        }

        drawIntersectionLine(group, lineX, centerY - Math.sqrt(chSquared));
        if (chSquared > 0)
            drawIntersectionLine(group, lineX, centerY + Math.sqrt(chSquared));

        // end TODO
        stage.setTitle("Geometry"); // Update Title as required
        stage.sizeToScene();
        stage.resizableProperty().setValue(Boolean.FALSE);
        stage.setScene(scene);
        stage.show();
    }

    private void drawIntersectionLine(Group group, double x, double y) {
        Circle intersectionPoint = new Circle(x, y, 3);
        Text coordinatesText = new Text(x + 10, y, String.format("(%.2f, %.2f)", x, y));

        group.getChildren().addAll(intersectionPoint, coordinatesText);
    }

    private double getLineX() {
        TextInputDialog dialog = new TextInputDialog();
        dialog.setTitle("Prompt");
        dialog.setHeaderText(null);
        dialog.setContentText("Enter line X");

//        final Optional<String> xCoordinate = dialog.showAndWait();
//        final double x = Double.parseDouble(xCoordinate.get());
//        if (x < 0 || x > sceneWidth) {
//            displayAlert("Error", String.format("Line X must be within [0, %2f]", sceneWidth), "", Alert.AlertType.ERROR);
//            return getLineX();
//        }

        double x;
        do {
            dialog.getEditor().clear();
            // Could throw error if Optional is empty
            x = Double.parseDouble(dialog.showAndWait().get());

            if (x < 0 || x > sceneWidth) {
                displayAlert("Error", String.format("Line X must be within [0, %2f]", sceneWidth), "", Alert.AlertType.ERROR);
            }
        } while (x < 0 || x > sceneWidth);
        return x;
    }

    private void displayAlert(String title, String header, String message, Alert.AlertType type) {
        Alert alert = new Alert(type);
        alert.setTitle(title);
        alert.setHeaderText(header);
        alert.setContentText(message);
        alert.showAndWait();
    }
}
