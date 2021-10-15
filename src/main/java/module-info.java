module com.pk {
    requires javafx.controls;
    requires javafx.fxml;


    opens com.pk to javafx.fxml;
    opens com.pk.frontend.menu to javafx.fxml;
    exports com.pk;
    exports com.pk.frontend.menu to javafx.fxml;
}