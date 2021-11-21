package com.pk.logic.exceptions;

public class JumpedOverAlreadyKilledPiece extends Exception {
    public JumpedOverAlreadyKilledPiece() {
    }

    public JumpedOverAlreadyKilledPiece(String errorMessage) {
        super(errorMessage);
    }

    public JumpedOverAlreadyKilledPiece(String errorMessage, Throwable err) {
        super(errorMessage, err);
    }
}
