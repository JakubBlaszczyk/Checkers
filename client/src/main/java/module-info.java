/** */
module com.pk {
  // requires transitive javafx.controls;
  // requires javafx.fxml;
  requires static lombok;
  requires org.slf4j;

  opens com.pk.lanserver;
  opens com.pk;
  // opens com.pk.frontend.menu;

  exports com.pk;
  exports com.pk.logic;
  exports com.pk.logic.exceptions;
  exports com.pk.lanserver;
  exports com.pk.lanserver.exceptions;
  exports com.pk.lanserver.models;
  // exports com.pk.frontend.menu;
}
