package com.pk;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.io.IOException;
import java.util.Locale;
import java.util.ResourceBundle;


public class App extends Application {

  public static Stage stage;
  public static ResourceBundle translations;
  public static Locale locale;



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

    public static void showBoardScene() throws IOException {
      locale = new Locale("pl_PL");
      translations = ResourceBundle.getBundle("translations", locale);
      Parent root = FXMLLoader.load(App.class.getClassLoader().getResource("BoardView.fxml"), translations);
      stage.setScene(new Scene(root, 800, 825));
      stage.show();
    }



    public static void main(String[] args) {
        launch(args);
    }
}
