package com.pk;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.util.Locale;
import java.util.ResourceBundle;

/**
 * Hello world!
 *
 */
public class App extends Application {

  public Stage stage;
  public ResourceBundle translations;
  public Locale locale;

  @Override
  public void start(Stage primaryStage) throws Exception {
    stage = primaryStage;
    locale = new Locale("pl_PL");
    translations = ResourceBundle.getBundle("translations", locale);
    Parent root = FXMLLoader.load(getClass().getClassLoader().getResource("MainMenuView.fxml"), translations);
    primaryStage.setTitle("Checkers");
    primaryStage.setScene(new Scene(root, 800, 800));
    primaryStage.show();
  }

  public static void main(String[] args) {
    launch(args);
  }
}
