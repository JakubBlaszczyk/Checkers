package com.pk.frontend.checkers;

import static com.pk.frontend.board.BoardController.TILE_SIZE;

import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.scene.shape.Ellipse;
import javafx.scene.shape.SVGPath;

public class Piece extends StackPane {

  private PieceType type;

  private double mouseX;
  private double mouseY;
  private double oldX;
  private double oldY;
  public SVGPath heart;
  public boolean isQueen;

  public void makeQueen() {
    this.getChildren().remove(this.heart);
    isQueen = true;
  }

  public PieceType getType() {
    return type;
  }

  public double getOldX() {
    return oldX;
  }

  public double getOldY() {
    return oldY;
  }

  public Piece(PieceType type, int x, int y) {
    isQueen = false;
    this.type = type;

    move(x, y);
    heart = new SVGPath();
    heart.setContent(
        "M23.6,0c-3.4,0-6.3,2.7-7.6,5.6C14.7,2.7,11.8,0,8.4,0C3.8,0,0,3.8,0,8.4c0,9.4,9.5,11.9,16,21.2c6.1-9.3,16-12.1,16-21.2C32,3.8,28.2,0,23.6,0z");

    heart.setFill(Color.BLACK);

    heart.setStroke(Color.BLACK);
    heart.setStrokeWidth(TILE_SIZE * 0.03);

    heart.setTranslateX((TILE_SIZE - TILE_SIZE * 0.3125 * 2) / 2);
    heart.setTranslateY((TILE_SIZE - TILE_SIZE * 0.26 * 2) / 2 + TILE_SIZE * 0.07);

    Ellipse ellipse = new Ellipse(TILE_SIZE * 0.3125, TILE_SIZE * 0.26);
    ellipse.setFill(type == PieceType.BLACK ? Color.valueOf("#666666") : Color.valueOf("#fff9f4"));

    ellipse.setStroke(Color.BLACK);
    ellipse.setStrokeWidth(TILE_SIZE * 0.03);

    ellipse.setTranslateX((TILE_SIZE - TILE_SIZE * 0.3125 * 2) / 2);
    ellipse.setTranslateY((TILE_SIZE - TILE_SIZE * 0.26 * 2) / 2);

    getChildren().addAll(ellipse, heart);

    setOnMousePressed(
        e -> {
          mouseX = e.getSceneX();
          mouseY = e.getSceneY();
        });

    setOnMouseDragged(e -> relocate(e.getSceneX() - mouseX + oldX, e.getSceneY() - mouseY + oldY));
  }

  public void move(int x, int y) {
    oldX = x * TILE_SIZE;
    oldY = y * TILE_SIZE;
    relocate(oldX, oldY);
  }

  public void abortMove() {
    relocate(oldX, oldY);
  }
}
