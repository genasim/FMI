package illusion;

import javafx.application.Application;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.paint.Color;
import javafx.scene.shape.Line;
import javafx.stage.Stage;

import java.util.Arrays;
// Abbreviaton key:       fx-drawing-main
// Template description: JavaFX App class for drawing
// Variables: CLASS_NAME must be assigned clipboard() expression
// 1. Create a Java class
// 2. Copy the class name in the Clipboard (^C)
// 3, Overwrite all  the class contents by running this Live template
// 4. Right-click the class name (should be the same as in the originally created class)
// 5. Select Show Content actions and execute Set package name to ...<your package name> 

public class LineIllusion extends Application {

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage stage) throws Exception {
        Group group = new Group();
        final double size = 500;
        Scene scene = new Scene(group, size, size);

        final int count = 25; // Number of lines per side
        final double step = size / count; // Step size for spacing lines

        for (int i = 0; i <= count; i++) {
            Line line1 = new Line(i * step, 0, 0, size - i * step);
            line1.setStroke(Color.ORANGE);

            Line line2 = new Line(size - i * step, 0, size, size - i * step);
            line2.setStroke(Color.ORANGE);

            Line line3 = new Line(i * step, size, size, size - i * step);
            line3.setStroke(Color.ORANGE);

            Line line4 = new Line(size - i * step, size, 0, size - i * step);
            line4.setStroke(Color.ORANGE);

            group.getChildren().addAll(line1, line2, line3, line4);
        }

        stage.setTitle("Lines illusion");
        stage.sizeToScene();
        stage.resizableProperty().setValue(Boolean.FALSE);
        stage.setScene(scene);
        stage.show();
    }
}
