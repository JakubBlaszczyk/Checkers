package com.pk.frontend.board;

import com.pk.database.Database;
import com.pk.frontend.checkers.*;
import com.pk.logic.ImprovedLogic;
import com.pk.logic.Indices;
import com.pk.logic.Logic;
import com.pk.logic.exceptions.IllegalArgument;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Group;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;
import javafx.scene.image.Image;
import javafx.scene.layout.Pane;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Locale;
import java.util.ResourceBundle;

@Slf4j
public class BoardController {
  public static final int TILE_SIZE = 100;
  public static final int WIDTH = 8;
  public static final int HEIGHT = 8;

  private Tile[][] board = new Tile[WIDTH][HEIGHT];

  private Group tileGroup = new Group();
  private Group pieceGroup = new Group();

  Locale locale;
  ResourceBundle bundle;

  @FXML
  private StackPane stackPane;
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
  private Button startButton;
  @FXML
  private Label whiteWin;
  @FXML
  private Label blackWin;

  private Logic logic;

  private Database database;

  @FXML
  public void initialize(){
    blackWin.setVisible(false);
    whiteWin.setVisible(false);
  }

  public void createContent(ActionEvent actionEvent) throws IllegalArgument, SQLException {
    logic = new ImprovedLogic(HEIGHT, 3);
    database = new Database("CheckersDatabase.db");
    database.insertIntoGame("player1", "player2");
    Pane root = new Pane();
    root.setPrefSize(WIDTH * TILE_SIZE, HEIGHT * TILE_SIZE);
    root.getChildren().addAll(tileGroup, pieceGroup);

    for (int y = 0; y < HEIGHT; y++) {
      for (int x = 0; x < WIDTH; x++) {
        Tile tile = new Tile((x + y) % 2 == 0, x, y);
        board[x][y] = tile;

        tileGroup.getChildren().add(tile);

        Piece piece = null;

        if (y <= 2 && (x + y) % 2 == 0) {
          piece = makePiece(PieceType.BLACK, x, y);
        }

        if (y >= 5 && (x + y) % 2 == 0) {
          piece = makePiece(PieceType.WHITE, x, y);
        }

        if (piece != null) {
          tile.setPiece(piece);
          pieceGroup.getChildren().add(piece);
        }
      }
    }
    stackPane.getChildren().add(root);
  }

  private Piece makePiece(PieceType type, int x, int y) {
    Piece piece = new Piece(type, x, y);

    piece.setOnMouseReleased(e -> {
      int newX = toBoard(piece.getLayoutX());
      int newY = toBoard(piece.getLayoutY());

      MoveResult result;

      int x0 = toBoard(piece.getOldX());
      int y0 = toBoard(piece.getOldY());
      result = logic.update(newX, newY, x0, y0);
      log.info("newX: {} | newY: {} | x0: {} | y0: {}", newX, newY, x0, y0);
      log.info("{}", logic.toString());

      switch (result.getType()) {
        case NONE:
          piece.abortMove();
          break;
        case NORMAL:
          piece.move(newX, newY);
          board[x0][y0].setPiece(null);
          board[newX][newY].setPiece(piece);
          database.insertIntoMapHistory(1, x0, y0, newX, newY);
          break;
        case KILL:
          piece.move(newX, newY);
          board[x0][y0].setPiece(null);
          board[newX][newY].setPiece(piece);

          Indices indices = result.getIndices();
          log.info("indiX: {} | indiY: {}", indices.getX(), indices.getY());
          log.info("board: ", board[indices.getX()][indices.getY()].getPiece());
          pieceGroup.getChildren().remove(board[indices.getX()][indices.getY()].getPiece());
          board[indices.getX()][indices.getY()].setPiece(null);
          database.insertIntoMapHistory(1, x0, y0, newX, newY);
          break;
        case MANDATORY_KILL:
          piece.abortMove();
          break;
      }

      Integer blackPieces = 0;
      Integer whitePieces = 0;

      for(Tile[] row : board){
        for(Tile tile : row){
          if(tile != null && tile.hasPiece() && tile.getPiece().getType().equals(PieceType.BLACK)){
            blackPieces++;
          }
          else if(tile != null && tile.hasPiece() && tile.getPiece().getType().equals(PieceType.WHITE)){
            whitePieces++;
          }
        }
      }

      if(blackPieces.equals(0)){
        log.info("GAME OVER - White wins");
        stackPane.getChildren().clear();
        stackPane.getChildren().add(whiteWin);
        stackPane.getChildren().add(startButton);
        whiteWin.setVisible(true);
      }
      else if(whitePieces.equals(0)){
        log.info("GAME OVER - Black wins");
        stackPane.getChildren().clear();
        stackPane.getChildren().add(blackWin);
        stackPane.getChildren().add(startButton);
        blackWin.setVisible(true);
      }
    });


    return piece;
  }

  private int toBoard(double pixel) {
    return (int)(pixel + TILE_SIZE / 2) / TILE_SIZE;
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
    whiteWin.setText(bundle.getString("whiteWin"));
    blackWin.setText(bundle.getString("blackWin"));
  }

  public void showCreators() throws IOException {
    Stage stage = new Stage();
    Parent root = FXMLLoader.load(getClass().getClassLoader().getResource("CreatorsView.fxml"));
    stage.setScene(new Scene(root,600,400));
    stage.getIcons().add(new Image("https://i.ibb.co/yNH0t4d/icon.png"));
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
    Stage stage = (Stage) startButton.getScene().getWindow();
    stage.close();
  }

  public void showMenu() throws IOException {
    Stage oldStage = (Stage) startButton.getScene().getWindow();
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
}
