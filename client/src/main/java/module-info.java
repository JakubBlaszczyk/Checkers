module com.pk {
    requires javafx.graphics;
    requires javafx.fxml;
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
    exports com.pk.frontend.checkers;
  exports com.pk.frontend.board;
  opens com.pk.frontend.board;
}
