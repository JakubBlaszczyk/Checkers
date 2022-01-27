package com.pk.frontend.menu;


import com.pk.App;
import com.pk.frontend.board.BoardController;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.css.converter.ColorConverter;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.geometry.Insets;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.image.Image;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;
import javafx.stage.Stage;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Locale;
import java.util.ResourceBundle;

/**
 * Used to handle GUI operations
 */
public class MainMenuController {

    Locale locale;
    ResourceBundle bundle;

    @FXML
    private Menu game;
    @FXML
    private MenuItem returnToMenu;
    @FXML
    private MenuItem leaveGame;
    @FXML
    private Menu help;
    @FXML
    private MenuItem rules;
    @FXML
    private MenuItem creators;
    @FXML
    private Menu language;
    @FXML
    private MenuItem polish;
    @FXML
    private MenuItem english;
    @FXML
    private Button hotseat;
    @FXML
    private Button multiplayer;
    @FXML
    private Button exit;
    @FXML
    private StackPane stackPane;

    private BoardController boardController = new BoardController();


    public void switchLanguageToEnglish(){
        setLanguage("en_US");
    }

    public void switchLanguageToPolish(){
        setLanguage("pl_PL");
    }

    private void setLanguage(String lang){
        locale = new Locale(lang);
        bundle = ResourceBundle.getBundle("translations", locale);
        game.setText(bundle.getString("game"));
        returnToMenu.setText(bundle.getString("returnToMenu"));
        leaveGame.setText(bundle.getString("leaveGame"));
        help.setText(bundle.getString("help"));
        rules.setText(bundle.getString("rules"));
        creators.setText(bundle.getString("creators"));
        language.setText(bundle.getString("language"));
        polish.setText(bundle.getString("polish"));
        english.setText(bundle.getString("english"));
        hotseat.setText(bundle.getString("hotseat"));
        multiplayer.setText(bundle.getString("multiplayer"));
        exit.setText(bundle.getString("exit"));
    }

    public void showCreators() throws IOException {
        Stage stage = new Stage();
        Parent root = FXMLLoader.load(getClass().getClassLoader().getResource("CreatorsView.fxml"));
        stage.setScene(new Scene(root,600,400));
        stage.getIcons().add(new Image("https://i.ibb.co/yNH0t4d/icon.png"));
        stage.show();
    }

    public void showLobby(ActionEvent actionEvent){
        hotseat.setVisible(false);
        multiplayer.setVisible(false);
        exit.setVisible(false);
        ListView<String> gamesList = new ListView<String>();
        ArrayList<String> lista = new ArrayList<String>();
        lista.add("gra1");
        lista.add("gra2");
        ObservableList<String> observableList = FXCollections.observableList(lista);
        gamesList.setItems(observableList);
        gamesList.setPrefSize(600,600);
        gamesList.setMaxSize(600,600);
        gamesList.setMinSize(600,600);
        TilePane tilePane = new TilePane(gamesList);
        tilePane.setPrefSize(600,600);
        tilePane.setMaxSize(600,600);
        tilePane.setMinSize(600,600);
        stackPane.getChildren().add(tilePane);

    }

    public void showBoard(ActionEvent actionEvent) throws IOException {
      hotseat.setVisible(false);
      multiplayer.setVisible(false);
      exit.setVisible(false);
      App.showBoardScene();
    }
}
