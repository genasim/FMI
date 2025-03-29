package problem1;

import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;

public class GcdController {

    @FXML
    private Button btnCompute;

    @FXML
    private Button btnQuit;

    @FXML
    private Label lblA;

    @FXML
    private Label lblB;

    @FXML
    private Label lblGcd;

    @FXML
    private TextField txtA;

    @FXML
    private TextField txtB;

    @FXML
    private TextField txtGcd;

    @FXML
    void onComputeClick(ActionEvent event) {
        int a = Integer.parseInt(txtA.getText());
        int b = Integer.parseInt(txtB.getText());

        int gcd = gcd(a, b);
        txtGcd.setText(String.valueOf(gcd));
    }

    @FXML
    void onQuitClick(ActionEvent event) {
        Platform.exit();
    }

    private static int gcd(int a, int b) {
        if (b == 0) return a;
        return gcd(b, a % b);
    }
}
