package com.pk.logic.exceptions;

public class BadBoardGiven extends Exception {
    public BadBoardGiven() {
    }

    public BadBoardGiven(String errorMessage) {
        super(errorMessage);
    }

    public BadBoardGiven(String errorMessage, Throwable err) {
        super(errorMessage, err);
    }
}

