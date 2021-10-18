/**
 * 
 */
module com.pk {
    requires transitive javafx.controls;
    requires javafx.fxml;
    requires static lombok;
    requires org.slf4j;

    opens com.pk.server;
    opens com.pk;
    opens com.pk.frontend.menu;
    exports com.pk;
    exports com.pk.logic;
    exports com.pk.logic.exceptions;
    exports com.pk.server;
    exports com.pk.server.exceptions;
    exports com.pk.server.models;
    exports com.pk.frontend.menu;
}