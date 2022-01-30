package com.pk.frontend.checkers;

import com.pk.frontend.board.BoardController;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;


public class Tile extends Rectangle {

    private Piece piece;

    public boolean hasPiece() {
        return piece != null;
    }

    public Piece getPiece() {
        return piece;
    }

    public void setPiece(Piece piece) {
        this.piece = piece;
    }

    public Tile(boolean light, int x, int y) {
        setWidth(BoardController.TILE_SIZE);
        setHeight(BoardController.TILE_SIZE);

        relocate(x * BoardController.TILE_SIZE, y * BoardController.TILE_SIZE);

        setFill(light ? Color.valueOf("#ffffd2") : Color.valueOf("#ffc0cb"));
    }
}
