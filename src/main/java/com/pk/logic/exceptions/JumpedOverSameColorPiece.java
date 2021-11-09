package com.pk.logic.exceptions;

public class JumpedOverSameColorPiece extends Exception {
    public JumpedOverSameColorPiece() {
    }

    public JumpedOverSameColorPiece(String errorMessage) {
        super(errorMessage);
    }

    public JumpedOverSameColorPiece(String errorMessage, Throwable err) {
        super(errorMessage, err);
    }
}
