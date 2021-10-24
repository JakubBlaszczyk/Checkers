package com.pk.logic.exceptions;

public class OverlappingPieces extends Exception {
    public OverlappingPieces() {
    }

    public OverlappingPieces(String errorMessage) {
        super(errorMessage);
    }

    public OverlappingPieces(String errorMessage, Throwable err) {
        super(errorMessage, err);
    }
}