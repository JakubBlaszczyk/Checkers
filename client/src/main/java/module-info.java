module com.pk {
    requires javafx.controls;
    requires javafx.graphics;
    requires javafx.fxml;
    requires javafx.swing;
    requires javafx.media;
    requires transitive javafx.controls;
    requires static lombok;
    requires org.slf4j;
    requires java.sql;
  
    opens com.pk.lanserver;
    opens com.pk;
    opens com.pk.frontend.menu;
  
    exports com.pk;
    exports com.pk.logic;
    exports com.pk.logic.exceptions;
    exports com.pk.lanserver;
    exports com.pk.lanserver.exceptions;
    exports com.pk.lanserver.models;
    exports com.pk.frontend.menu;
}