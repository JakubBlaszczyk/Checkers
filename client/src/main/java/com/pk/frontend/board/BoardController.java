package com.pk.frontend.board;

import com.pk.frontend.checkers.*;
import com.pk.logic.ImprovedLogic;
import com.pk.logic.Indices;
import com.pk.logic.Logic;
import com.pk.logic.exceptions.IllegalArgument;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.Group;
import javafx.scene.layout.Pane;
import javafx.scene.layout.StackPane;
import lombok.extern.slf4j.Slf4j;

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

  private Logic logic;


  public void createContent(ActionEvent actionEvent) throws IllegalArgument {
    logic = new ImprovedLogic(HEIGHT, 3);
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

//      if (newX < 0 || newY < 0 || newX >= WIDTH || newY >= HEIGHT) {
//        result = new MoveResult(MoveType.NONE);
//      } else {
//        result = tryMove(piece, newX, newY);
//      }


      int x0 = toBoard(piece.getOldX());
      int y0 = toBoard(piece.getOldY());
      result = logic.update(newX, newY, x0, y0);
      log.info("newX: {} | newY: {} | x0: {} | y0: {}", newX, newY, x0, y0);

      switch (result.getType()) {
        case NONE:
          piece.abortMove();
          break;
        case NORMAL:
          piece.move(newX, newY);
          board[x0][y0].setPiece(null);
          board[newX][newY].setPiece(piece);
          break;
        case KILL:
          piece.move(newX, newY);
          board[x0][y0].setPiece(null);
          board[newX][newY].setPiece(piece);

          Indices indices = result.getIndices();
          board[indices.getX()][indices.getY()].setPiece(null);
          pieceGroup.getChildren().remove(board[indices.getX()][indices.getY()].getPiece());
          break;
      }
    });

    return piece;
  }

  private MoveResult tryMove(Piece piece, int newX, int newY) {
    if (board[newX][newY].hasPiece() || (newX + newY) % 2 == 0) {
      return new MoveResult(MoveType.NONE);
    }

    int x0 = toBoard(piece.getOldX());
    int y0 = toBoard(piece.getOldY());

    if (Math.abs(newX - x0) == 1 && newY - y0 == piece.getType().moveDir) {
      return new MoveResult(MoveType.NORMAL);
    } else if (Math.abs(newX - x0) == 2 && newY - y0 == piece.getType().moveDir * 2) {

      int x1 = x0 + (newX - x0) / 2;
      int y1 = y0 + (newY - y0) / 2;

      if (board[x1][y1].hasPiece() && board[x1][y1].getPiece().getType() != piece.getType()) {
        return new MoveResult(MoveType.KILL, new Indices(x1, y1));
      }
    }

    return new MoveResult(MoveType.NONE);
  }

  private int toBoard(double pixel) {
    return (int)(pixel + TILE_SIZE / 2) / TILE_SIZE;
  }

  public void showCreators(ActionEvent actionEvent) {
  }

  public void switchLanguageToPolish(ActionEvent actionEvent) {
  }

  public void switchLanguageToEnglish(ActionEvent actionEvent) {
  }
}
