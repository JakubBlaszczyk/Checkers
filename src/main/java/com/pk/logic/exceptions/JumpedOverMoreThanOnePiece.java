package com.pk.logic.exceptions;

public class JumpedOverMoreThanOnePiece extends Exception {
    public JumpedOverMoreThanOnePiece() {
    }

    public JumpedOverMoreThanOnePiece(String errorMessage) {
        super(errorMessage);
    }

    public JumpedOverMoreThanOnePiece(String errorMessage, Throwable err) {
        super(errorMessage, err);
    }
}
