package com.pk.frontend.menu;


import com.pk.database.Database;
import com.pk.database.Game;
import com.pk.database.MapHistory;
import com.pk.frontend.board.BoardController;
import com.pk.lanserver.ServerDetails;
import com.pk.lanserver.WebTcpClient;
import com.pk.lanserver.exceptions.InvitationRejected;
import com.pk.lanserver.exceptions.MoveRejected;
import com.pk.lanserver.models.Invite;
import com.pk.lanserver.models.Move;
import javafx.animation.PauseTransition;
import javafx.application.Platform;
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
import javafx.util.Duration;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.SQLException;
import java.util.*;
import java.util.concurrent.*;

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
    @FXML
    private Label insertUsername;
    @FXML
    private TextField username;
    @FXML
    private Button acceptUsername;
    @FXML
    private Label insertPort;
    @FXML
    private TextField localPort;
    @FXML
    private Button waitButton;
    @FXML
    private Label waiting;

    private Database database;

    private BoardController boardController = new BoardController();

    public WebTcpClient wts;

    public BlockingQueue<String> bQS;

    public BlockingQueue<Move> bQM;

    public BlockingQueue<Invite> bQI;

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
        insertUsername.setVisible(false);
        username.setVisible(false);
        acceptUsername.setVisible(false);
        insertPort.setVisible(false);
        localPort.setVisible(false);
        waitButton.setVisible(false);
        waiting.setVisible(false);
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
        insertUsername.setText(bundle.getString("insertUsername"));
        acceptUsername.setText(bundle.getString("acceptUsername"));
        insertPort.setText(bundle.getString("insertPort"));
        waitButton.setText(bundle.getString("waitButton"));
        waiting.setVisible(bundle.containsKey("waiting"));
    }

    public void showCreators() throws IOException {
        Stage stage = new Stage();
        Parent root = FXMLLoader.load(getClass().getClassLoader().getResource("CreatorsView.fxml"));
        stage.setScene(new Scene(root,600,400));
        stage.getIcons().add(new Image("https://i.ibb.co/yNH0t4d/icon.png"));
        stage.show();
    }

    public void showLobby(){
        username.setVisible(false);
        insertUsername.setVisible(false);
        acceptUsername.setVisible(false);
        joinGame.setVisible(true);
        newGame.setVisible(true);
    }

    public void showBoard() throws IOException {
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

    public void initializeServer(String address, Integer port, String nickname) throws IOException {
        bQI = new LinkedBlockingQueue<>();
        bQS = new LinkedBlockingQueue<>();
        bQM = new LinkedBlockingQueue<>();
        wts = new WebTcpClient(bQI, bQS, bQM, address, port, Base64.getEncoder().encodeToString(nickname.getBytes(StandardCharsets.UTF_8)),
                "dGVzdA==");
        ExecutorService executorService = Executors.newFixedThreadPool(2);
        executorService.submit(wts);
    }

    public void inputUsername(){
        hotseat.setVisible(false);
        multiplayer.setVisible(false);
        history.setVisible(false);
        exit.setVisible(false);
        username.setVisible(true);
        acceptUsername.setVisible(true);
        insertUsername.setVisible(true);
    }

    public void setUsername(){
        log.info("username: " + username.getText());
        showLobby();
    }

    public void createGame(){
        newGame.setVisible(false);
        joinGame.setVisible(false);
        insertIP.setVisible(true);
        localIP.setVisible(true);
        generateCode.setVisible(true);
    }

    public void generateCode() throws ExecutionException, InterruptedException, IOException {
        initializeServer(localIP.getText(), 6969, username.getText());
        String code = wts.getInviteCode().get();
        inviteCode.setText(code);
        inviteCode.setVisible(true);
        waitButton.setVisible(true);
    }

    public void showWaitingScreen() throws InterruptedException, IOException {

        TimeUnit.SECONDS.sleep(2);
        for (; ; ) {
            TimeUnit.SECONDS.sleep(1);
            if (!bQI.isEmpty()) {
                break;
            }
        }
        Invite inv = (Invite) bQI.poll();
        wts.acceptInvitation(inv.getCode());
        wts.chatSendMsg("my turn");
        TimeUnit.SECONDS.sleep(1);
        ServerDetails.setWts(wts);
        ServerDetails.setbQI(bQI);
        ServerDetails.setbQM(bQM);
        ServerDetails.setbQS(bQS);
        showBoard();
    }

    public void joinToGame(){
        newGame.setVisible(false);
        joinGame.setVisible(false);
        localIP.setVisible(true);
        insertIP.setVisible(true);
        codeInput.setVisible(true);
        insertCode.setVisible(true);
        join.setVisible(true);
    }

    public void joinActiveGame() throws IOException, ExecutionException, InterruptedException, InvitationRejected, MoveRejected {
        bQI = new LinkedBlockingQueue<>();
        bQS = new LinkedBlockingQueue<>();
        bQM = new LinkedBlockingQueue<>();
        wts = new WebTcpClient(bQI, bQS, bQM, localIP.getText(), 6969, Base64.getEncoder().encodeToString(username.getText().getBytes(StandardCharsets.UTF_8)), "dDI=");
        ExecutorService executorService = Executors.newFixedThreadPool(2);
        executorService.submit(wts);
        log.info("Got invite code: {}", wts.getInviteCode().get());
        log.info("Got active players: {}", wts.getActivePlayers().get());
        TimeUnit.SECONDS.sleep(1);
        wts.invite(codeInput.getText()).get();
        wts.chatSendMsg("your turn");
        showBoard();
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
