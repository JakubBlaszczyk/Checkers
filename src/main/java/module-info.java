module com.pk {
    requires javafx.controls;
    requires javafx.fxml;


    opens com.pk to javafx.fxml;
    exports com.pk;
}