package com.pk.logic.exceptions;

public class MoreThanOneTileMove extends Exception {
    public MoreThanOneTileMove() {
    }

    public MoreThanOneTileMove(String errorMessage) {
        super(errorMessage);
    }

    public MoreThanOneTileMove(String errorMessage, Throwable err) {
        super(errorMessage, err);
    }
}