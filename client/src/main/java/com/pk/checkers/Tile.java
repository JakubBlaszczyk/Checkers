package com.pk.checkers;

import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;
import com.pk.App;


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
        setWidth(App.TILE_SIZE);
        setHeight(App.TILE_SIZE);

        relocate(x * App.TILE_SIZE, y * App.TILE_SIZE);

        setFill(light ? Color.valueOf("#ffffd2") : Color.valueOf("#ffc0cb"));
    }
}
