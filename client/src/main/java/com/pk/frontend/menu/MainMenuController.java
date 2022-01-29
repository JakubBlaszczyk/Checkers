package com.pk.frontend.menu;


import com.pk.database.Database;
import com.pk.database.Game;
import com.pk.database.MapHistory;
import com.pk.frontend.board.BoardController;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.image.Image;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.*;
import javafx.stage.Stage;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.sql.SQLException;
import java.util.*;

/**
 * Used to handle GUI operations
 */
@Slf4j
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
    @FXML
    private Button joinGame;
    @FXML
    private Button newGame;
    @FXML
    private Label insertCode;
    @FXML
    private TextField codeInput;
    @FXML
    private Button join;
    @FXML
    private Label insertIP;
    @FXML
    private TextField localIP;
    @FXML
    private Button generateCode;
    @FXML
    private TextField inviteCode;
    @FXML
    private Button history;

    private Database database;

    private BoardController boardController = new BoardController();

    @FXML
    public void initialize(){
        joinGame.setVisible(false);
        newGame.setVisible(false);
        insertCode.setVisible(false);
        codeInput.setVisible(false);
        join.setVisible(false);
        localIP.setVisible(false);
        insertIP.setVisible(false);
        generateCode.setVisible(false);
        inviteCode.setVisible(false);
    }

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
        joinGame.setText(bundle.getString("joinGame"));
        newGame.setText(bundle.getString("createGame"));
        insertCode.setText(bundle.getString("insertCode"));
        join.setText(bundle.getString("joinGame"));
        insertIP.setText(bundle.getString("insertIP"));
        generateCode.setText(bundle.getString("generateCode"));
        history.setText(bundle.getString("history"));
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
        history.setVisible(false);
        exit.setVisible(false);
        joinGame.setVisible(true);
        newGame.setVisible(true);
    }

    public void showBoard(ActionEvent actionEvent) throws IOException {
        Stage oldStage = (Stage) exit.getScene().getWindow();
        Stage stage = new Stage();
        locale = new Locale("pl_PL");
        bundle = ResourceBundle.getBundle("translations", locale);
        Parent root = FXMLLoader.load(getClass().getClassLoader().getResource("BoardView.fxml"), bundle);
        stage.getIcons().add(new Image("https://i.ibb.co/yNH0t4d/icon.png"));
        stage.setScene(new Scene(root, 800, 825));
        oldStage.close();
        stage.show();
    }

    public void showRules() throws IOException {
        Stage stage = new Stage();
        Parent root = FXMLLoader.load(getClass().getClassLoader().getResource("RulesView.fxml"));
        stage.setScene(new Scene(root,800,600));
        stage.getIcons().add(new Image("https://i.ibb.co/yNH0t4d/icon.png"));
        stage.show();
    }

    public void closeWindow(){
        Stage stage = (Stage) exit.getScene().getWindow();
        stage.close();
    }

    public void showMenu() throws IOException {
        Stage oldStage = (Stage) exit.getScene().getWindow();
        Stage stage = new Stage();
        locale = new Locale("pl_PL");
        bundle = ResourceBundle.getBundle("translations", locale);
        Parent root = FXMLLoader.load(getClass().getClassLoader().getResource("MainMenuView.fxml"), bundle);
        stage.setTitle("Checkers");
        stage.getIcons().add(new Image("https://i.ibb.co/yNH0t4d/icon.png"));
        stage.setScene(new Scene(root, 800, 800));
        oldStage.close();
        stage.show();
    }

    public void createGame(){
        newGame.setVisible(false);
        joinGame.setVisible(false);
        insertIP.setVisible(true);
        localIP.setVisible(true);
        generateCode.setVisible(true);
    }

    public void generateCode(){
        inviteCode.setText("KOD ARKADIUSZA");
        inviteCode.setVisible(true);
    }

    public void joinToGame(){
        newGame.setVisible(false);
        joinGame.setVisible(false);
        codeInput.setVisible(true);
        insertCode.setVisible(true);
        join.setVisible(true);
    }

    public void showHistory() throws SQLException {
        hotseat.setVisible(false);
        multiplayer.setVisible(false);
        history.setVisible(false);
        exit.setVisible(false);

        database = new Database("CheckersDatabase.db");
        List<Game> game = database.selectFromGame();
        ListView<String> gamesList = new ListView<>();
        ArrayList<String> lista = new ArrayList<>();
        for(Game oneGame : game){
            lista.add(oneGame.toString());
        }
        ObservableList<String> observableList = FXCollections.observableList(lista);
        gamesList.setItems(observableList);
        gamesList.setPrefSize(600,600);
        gamesList.setMaxSize(600,600);
        gamesList.setMinSize(600,600);
        TilePane tilePane = new TilePane(gamesList);
        tilePane.setPrefSize(600,600);
        tilePane.setMaxSize(600,600);
        tilePane.setMinSize(600,600);
        gamesList.setOnMouseClicked(new EventHandler<MouseEvent>() {
            @Override
            public void handle(MouseEvent mouseEvent) {
                String gameRow = gamesList.getSelectionModel().getSelectedItem();
                showMoves(Integer.parseInt(gameRow.substring(8, gameRow.indexOf(","))), tilePane);
            }
        });
        stackPane.getChildren().add(tilePane);
    }

    public void showMoves(int gameId, TilePane tilePane){
        stackPane.getChildren().remove(tilePane);
        List<MapHistory> mapHistoryList = database.selectFromMapHistory(gameId);
        ListView<String> movesList = new ListView<>();
        ArrayList<String> list = new ArrayList<>();
        for(MapHistory mapHistory : mapHistoryList){
            list.add(mapHistory.toString());
        }
        ObservableList<String> observableList = FXCollections.observableList(list);
        movesList.setItems(observableList);
        movesList.setPrefSize(600,600);
        movesList.setMaxSize(600,600);
        movesList.setMinSize(600,600);
        TilePane movesTilePane = new TilePane(movesList);
        movesTilePane.setPrefSize(600,600);
        movesTilePane.setMaxSize(600,600);
        movesTilePane.setMinSize(600,600);
        stackPane.getChildren().add(movesTilePane);
    }
}
