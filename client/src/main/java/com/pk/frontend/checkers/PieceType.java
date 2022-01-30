package com.pk.frontend.checkers;


public enum PieceType {
    BLACK(1), WHITE(-1);

    public final int moveDir;

    PieceType(int moveDir) {
        this.moveDir = moveDir;
    }
}
