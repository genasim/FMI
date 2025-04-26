package problem3;

import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;

import java.util.Arrays;

public class InsertionController {
    @FXML
    private Button btnQuit;

    @FXML
    private Button btnSort;

    @FXML
    private TextArea txaInput;

    @FXML
    private TextArea txaSorted;

    @FXML
    private TextField txtOrder;

    @FXML
    void btnQuitOnAction(ActionEvent event) {
        Platform.exit();
    }

    @FXML
    void btnSortOnAction(ActionEvent event) {
        String input = txaInput.getText().trim();
        String[] tokens = input.split("\\s+");

        int[] nums = new int[tokens.length];
        for (int i = 0; i < tokens.length; i++) {
            nums[i] = Integer.parseInt(tokens[i]);
        }

        Arrays.sort(nums);
        if (txtOrder.getText().matches("[Yy]?")) {
            txaSorted.setText(Arrays.toString(nums));
            return;
        }

        final StringBuilder sb = new StringBuilder();
        sb.append("[");
        for (int i = nums.length - 1; i >= 0; i--) {
            sb.append(nums[i]);
            if (i != 0) sb.append(",");
        }
        sb.append("]");
        txaSorted.setText(sb.toString());
    }

    @FXML
    void initialize() {
        assert btnQuit != null : "fx:id=\"btnQuit\" was not injected: check your FXML file 'insertion-view.fxml'.";
        assert btnSort != null : "fx:id=\"btnSort\" was not injected: check your FXML file 'insertion-view.fxml'.";
        assert txaInput != null : "fx:id=\"txaInput\" was not injected: check your FXML file 'insertion-view.fxml'.";
        assert txaSorted != null : "fx:id=\"txaSorted\" was not injected: check your FXML file 'insertion-view.fxml'.";
        assert txtOrder != null : "fx:id=\"txtOrder\" was not injected: check your FXML file 'insertion-view.fxml'.";
    }
}
