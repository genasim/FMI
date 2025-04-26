package problem3;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Group;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.util.Objects;

public class InsertionApplication124 23 245 2				-12412	652523	-10000000	999999 extends Application {

    public static void main(java.lang.String[] args) {
        launch(args);
    }

    @java.lang.Override
    public void start(Stage stage) throws java.lang.Exception {
        Parent root = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("insertion-view.fxml")));
        Scene scene = new Scene(root);

        // TODO  Type code for Java FX drawing objects


        // end TODO
        stage.setTitle("Insertion Sort"); // Update Title as required
        stage.sizeToScene();
        stage.resizableProperty().setValue(java.lang.Boolean.FALSE);
        stage.setScene(scene);
        stage.show();
    }
}
