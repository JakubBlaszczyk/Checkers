package com.pk;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

/**
 * Hello world!
 *
 */
public class App extends Application {

  @Override
  public void start(Stage primaryStage) throws Exception {
    Parent root = FXMLLoader.load(getClass().getClassLoader().getResource("MainMenuView.fxml"));
    primaryStage.setTitle("Checkers");
    primaryStage.setScene(new Scene(root, 800, 800));
    primaryStage.show();
  }

  public static void main(String[] args) {
    launch(args);
  }
}
